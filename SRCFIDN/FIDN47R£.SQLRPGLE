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
100500040726     c
100600040726      * Stampa descrizione pezzi danneggiati/mancanti
100700040726     c                   if        I43sdm = 'S'
100800040726     c                   eval      P1Csdm = 'Si'
100900040726     c                   endif
101000990212     c
101100990212     c* Tipo C.A.
101200990212     c                   if        i43tca='A'
101300990212     c                   eval      p1dtca='APERTE         '
101400990212     c                   endif
101500990212     c                   if        i43tca='C'
101600990212     c                   eval      p1dtca='CHIUSE         '
101700990212     c                   endif
101800990212     c                   if        i43tca='E'
101900990212     c                   eval      p1dtca='APERTE E CHIUSE'
102000990316     c                   endif
102100990215     c*
102200990215     c* Memorizzo se sono state fatte selezioni sulla bolla
102300990215     c                   if        i43lnp>0 or i43lna>0 or
102400990316     c                             i43dsd>0 or i43dsa>0 or
102500990316     c                             i43ksm>0 or i43ksd>0 or
102600060125     c                             i43rsm<>*blanks or i43rsd<>*blanks  or
102700060125     c                             i43val<>*blanks
102800990215     c                   eval      WSELBOL='S'
102900060125     c
103000060125     c* Stampa richiesta spedizioni di valore
103100060125     c                   if        i43val='S'
103200060125     c                   eval      p1cval='Si'
103300060125     c                   endif
103400990316     c* Mittente
103500990316     c                   if        i43ksm>0
103600990322     c                   movel     i43ksm        wa43ksm
103700990316     C                   clear                   TIBS69DS
103800990316     c                   movel     i43ksm        I69kac
103900990316      *
104000990316     C                   CALL      'TIBS69R'
104100990316     C                   PARM                    tibs69DS
104200990316     C                   PARM                    DS_cnaco
104300990316     C                   PARM                    DS_cnind
104400990316     C                   PARM                    DS_cnclp
104500990316     C                   PARM                    DS_fncls
104600990316    2c                   if        o69err=' '
104700990316     c                   movel     acorag        p1dksm
104800990316    2c                   endif
104900990217     c                   endif
105000990322     c* Cerco l'ultima posizione piena della ragione sociale
105100990322     c                   if        i43rsm<>*blanks
105200990322     c     ' '           checkr    i43rsm        wPOSM
105300990322     c                   endif
105400990316     c* Destinatario
105500990316     c                   if        i43ksd>0
105600990322     c                   movel     i43ksd        wa43ksd
105700990316     C                   clear                   TIBS69DS
105800990316     c                   movel     i43ksd        I69kac
105900990316      *
106000990316     C                   CALL      'TIBS69R'
106100990316     C                   PARM                    tibs69DS
106200990316     C                   PARM                    DS_cnaco
106300990316     C                   PARM                    DS_cnind
106400990316     C                   PARM                    DS_cnclp
106500990316     C                   PARM                    DS_fncls
106600990316    2c                   if        o69err=' '
106700990316     c                   movel     acorag        p1dksd
106800990316    2c                   endif
106900990316     c                   endif
107000990322     c* Cerco l'ultima posizione piena della ragione sociale
107100990322     c                   if        i43rsd<>*blanks
107200990322     c     ' '           checkr    i43rsd        wPOSD
107300990322     c                   endif
107400990316     c
107500990316     c                   endif
107600990406     c
107700090320     c* impostio date di elaborazione con data apertuura o chiusura c.a.
107800090319     c                   move      i43amgdcd     wdal
107900000216     C                   IF        I43AMGDCA>0
108000090319     c                   move      i43amgdca     wal
108100000216     C                   ELSE
108200090319     c                   move      i43amgdcd     wal
108300000216     C                   endif
108400090320
108500090320     c                   if        wdal=0 and i43amgdhd>0
108600090320     c                   move      i43amgdhd     wdal
108700090320     C                   IF        I43AMGDhA>0
108800090320     c                   move      i43amgdha     wal
108900090320     C                   ELSE
109000090320     c                   move      i43amgdhd     wal
109100090320     C                   endif
109200090320     c                   endif
109300000216     c
109400090320     c* Se vengono creati i dati, li devo anche cancellare prima
109500090320     c                   if        i43crp='S'
109600090320     c* se elaborata una sola data cancello record "G"
109700090320     c* se elaborate più date cancello record "R"
109800090320     c                   if        wdal=wal
109900090320     c                   eval      ktie='G'
110000110912     c                   eval      ktieav='A'
110100090320     c                   else
110200090320     c                   eval      ktie='R'
110300110912     c                   eval      ktieav='B'
110400090320     c                   endif
110500110912     c
110600090320     c     ktie          setll     wfrdn000
110700090320     c     ktie          reade     wfrdn000                               32
110800990406     c                   dow       not *in32
110900990408     c                   delete    wfrdn000
111000090320     c     ktie          reade     wfrdn000                               32
111100090319     c                   enddo
111200110912     c
111300110912     c     ktieAV        setll     wfrdn000
111400110912     c     ktieAV        reade     wfrdn000                               32
111500110912     c                   dow       not *in32
111600110912     c                   delete    wfrdn000
111700110912     c     ktieAV        reade     wfrdn000                               32
111800110912     c                   enddo
111900090319     c                   endif
112000990429     c
112100990429     c* Uso interno
112200990429     c                   if        i43usi='S'
112300990429     c                   eval      p1cusi='Si'
112400990429     c                   else
112500990429     c                   eval      p1cusi='No'
112600990429     c                   endif
112700050708     c                   select
112800050708     c                   when      i43toi='S'
112900050708     c                   eval      p1ctoi='Si                    '
113000050708     c
113100050708     c                   when      i43toi='G'
113200050708     c                   eval      p1ctoi='Generali con dettaglio'
113300050708     c                   other
113400050708     c                   eval      i43toi=' '
113500050708     c                   endsl
113600050708     c
113700990429     c* crea dati per report
113800990429     c                   if        i43crp='S'
113900990429     c                   eval      p1ccrp='Si'
114000040726     c                   else
114100040726     c                   eval      p1ccrp='No'
114200990429     c                   endif
114300040906      * crea workfile con dettaglio
114400050906     c                   select
114500050906     c                   when      i43cfd='S'
114600040906     c                   eval      p1ccfd='Si'
114700050906     c                   when      i43cfd='P'
114800050906     c                   eval      p1ccfd='SI'
114900050906     c                   other
115000040906     c                   eval      p1ccfd='No'
115100050906     c                   endsl
115200990406     c
115300121219     c* file printer: se richiesto congelato  faccio ovrprtf
115400121219     C* ESEGUO OVRPRTF
115500121219     c                   if        i43psn='H'
115600121219     C                   Z-ADD     50            LUNG             15 5
115700121219     C                   MOVEL     *BLANKS       COMMAN           80
115800121219     C                   MOVEA     ovrprt        COMMAN
115900121219     C                   CALL      'QCMDEXC'                            H1
116000121219     C                   PARM                    COMMAN
116100121219     C                   PARM                    LUNG
116200121219     c                   endif
116300121219     C*
116400121219     C                   OPEN      FIDN44P
116500121219     C*
116600990212     C                   ENDSR
116700990219      *------------------------------------------------------------------------*
116800990219      * DECODIFICO TESTATA DISTRETTO E AREA
116900990219      *------------------------------------------------------------------------*
117000990219     C     DECODTES2     BEGSR
117100040204      *
117200990310     c* Area
117300040202if  2c                   if        orgcar<>p3care
117400990310     c                   eval      wbrka='1'
117500990312     c                   eval      wbrkn='1'
117600990323     c
117700990323     c                   if        p3care>0
117800990323     c                   eval      wnuovo='S'
117900990323     c                   endif
118000990323     c
118100990312     c                   EXSR      TOTAREA
118200040202e   2c                   endif
118300990310     c
118400990310     c* Distretto
118500040202if  2c                   if        orgfl3<>p3cdst
118600990309     c                   eval      wbrkd='1'
118700990323     c                   if        p3care>0
118800990323     c                   eval      wnuovo='S'
118900990323     c                   endif
119000990310     c                   EXSR      TOTDISTR
119100040202e   2c                   endif
119200990309     c
119300990219     c                   clear                   p3ddst
119400990219     c                   clear                   p3dare
119500990312     c                   clear                   p3dds2
119600990312     c                   clear                   p3dar2
119700990219     c                   movel     orgcar        p3care
119800990219     c                   movel     orgfl3        p3cdst
119900990219     c
120000990219     C                   movel     '17'          kCOD
120100990219     C                   movel(P)  orgfl3        kKEY
120200990219     C     ktab          CHAIN     TABEL                              31
120300990219     C  n31              movel     TBLUNI        p3ddst
120400990312     C  n31              movel     TBLUNI        p3dds2
120500990309     C  n31              movel     TBLUNI        ds17
120600990309     c   31              clear                   ds17
120700990219     c* Area
120800990219     C                   movel     '05'          kCOD
120900990219     C                   movel(P)  orgcar        kKEY
121000990219     C     ktab          CHAIN     TABEL                              31
121100990219     C  n31              movel     TBLUNI        p3dare
121200990312     C  n31              movel     TBLUNI        p3dar2
121300990309     C  n31              movel     TBLUNI        ds05
121400990309     c   31              clear                   ds05
121500990219     C                   ENDSR
121600990219      *------------------------------------------------------------------------*
121700990219      * LETTURA per stmapa
121800990219      *------------------------------------------------------------------------*
121900990219     c     LETTURE       BEGSR
122000990219     c* 2 ordinamenti :
122100990219     c* 1) per numero C.A.
122200990219     c                   select
122300990219     c                   when      i43ord='1'
122400990219     c                   exsr      LEGGICA
122500990219     c                   when      i43ord='2'
122600990219     c                   exsr      LEGGIRES
122700990219     c                   endsl
122800990219
122900990219     c* Stampo l'ultimo totale parziale
123000990219     c                   if        p3ctps>0
123100990219     c                   eval      dctfil=000
123200990219     c                   eval      dctaac=0000
123300990219     c                   eval      dcrpor=999
123400990219     c                   EXSR      ROTTURE
123500990219     c                   endif
123600990219     c                   endsr
123700981204      *------------------------------------------------------------------------*
123800990212      * LETTURA PER NUMERO C.A.
123900981204      *------------------------------------------------------------------------*
124000990212     C     LEGGICA       BEGSR
124100990212     c* Se c'e' anno e P.O. li uso altrimenti dall'inizio
124200990212     c                   select
124300990219     c                   when      i43aac>0 and kfil>0
124400990215     c     kdct1         chain     fndct11l                           30
124500990219     c                   when      i43aac>0 and kfil=0
124600990215     c     i43aac        chain     fndct01l                           30
124700990219     c                   when      kfil>0 and i43aac=0
124800990219     c     kfil          chain     fndct11l                           30
124900990212     c                   other
125000990215     c     *loval        setll     fndct11l
125100990215     c                   read      fndct11l                               30
125200990212     c                   endsl
125300990212     c**
125400040202do  1c                   DOW       not *in30
125500040202if  2c     dctatb        ifeq      ' '
125600990212     c                   EXSR      CONFRO
125700040202if  3c                   if        wconfr<>'N'
125800990215     c                   exsr      STAMPA
125900050906if  4c                   if           I43cfd = 'S'
126000050906     c                             or I43cfd = 'P'
126100040204     c                   exsr      Wrt_WFDDN
126200040204e   4c                   endif
126300040202e   3c                   endif
126400040202e   2c                   endif
126500990212     c
126600990212     c* Lettura fondo ciclo
126700990212     c                   select
126800990219     c                   when      i43aac>0 and kfil>0
126900990215     c     kdct1         reade     fndct11l                               30
127000990219     c                   when      i43aac>0 and kfil=0
127100990212     c     i43aac        reade     fndct01l                               30
127200990219     c                   when      kfil>0 and i43aac=0
127300990219     c     kfil          reade     fndct11l                               30
127400990212     c                   other
127500990215     c                   read      fndct11l                               30
127600990212     c                   endsl
127700990212     c
127800040202e   1c                   ENDDO
127900990212     c                   ENDSR
128000990217      *------------------------------------------------------------------------*
128100990218      * LETTURA PER P.O. reponsabile
128200990217      *------------------------------------------------------------------------*
128300990217     C     LEGGIRES      BEGSR
128400090402    0c                   dou       kfil=0
128500090323     c
128600090323     c* Se impostato il p.o. responsabile --> ok
128700090323     c*   altrimenti lo devo cercare
128800090402    1c                   if        i43prs>0
128900090323     c                   eval      kfil=i43prs
129000090323     c                   else
129100090323     c     kDCRcer       setgt     fndcr05l
129200090323     c     kteralfa      reade     fndcr05l
129300090402    2c                   if        not %eof(fndcr05l)
129400090323     c                   eval      kfil=dcrpor
129500090323     c                   else
129600090323     c                   clear                   kfil
129700090402    2c                   endif
129800090402    1c                   endif
129900090323     c
130000090323     c
130100090402    1c                   if        kfil>0
130200990503     c* mio faccio guidare dall'ordinamento dei tipi responsabilita'
130300990503     c     'A1'          setll     fndrr01l
130400990503     c     'A1'          reade     fndrr01l                               38
130500040202do  1c                   dow       not *in38
130600990504     c
130700090319     c     kdcr3         chain     fndcr05l                           30
130800990218     c
130900040202do  2c                   DOW       not *in30
131000040202if  3c     dcratb        ifeq      ' '
131100090319     c                   eval      ddcr01=dcrflo
131200090319     c
131300090319     c                   if        §DCRTER<=*zeros
131400090319     c                   eval      §dcrter=%editc(dcrpor:'X')
131500090319     c                   endif
131600090323     c                   eval      kter=%int(§dcrter)
131700090319     c
131800990310     c* Leggo le testate c.a.
131900990218     c     kdcrt         chain     fndct01l                           31
132000990218     c*
132100040202if  4c                   if        not *in31 and dctatb=' '
132200990218     c                   EXSR      CONFRO
132300040202if  5c                   if        wconfr<>'N'
132400990218     c                   exsr      STAMPA
132500050906if  6c                   if           I43cfd = 'S'
132600050906     c                             or I43cfd = 'P'
132700040204     c                   exsr      Wrt_WFDDN
132800040204e   6c                   endif
132900040202e   5c                   endif
133000990406     c
133100990406     c* Vedo se devo anche scrivere il file del report
133200990406     c                   if        simfel=0 and i43crp='S' and Wscrivi='S'
133300990406     c                   exsr      SCRIREP
133400990218     c                   endif
133500990406     c
133600040202e   4c                   endif
133700040202e   3c                   endif
133800990218     c
133900990218     c* Lettura fondo ciclo
134000090319     c     kdcr3         reade     fndcr05l                               30
134100040202e   2c                   enddo
134200990503     c
134300990503     c     'A1'          reade     fndrr01l                               38
134400040202e   1c                   enddo
134500090323     c
134600090402    1c                   endif
134700090402     c                   if        i43prs>0
134800090402     c                   clear                   kfil
134900090402     c                   endif
135000090323     c
135100090402e   0c                   enddo
135200990218     c
135300990217     c                   ENDSR
135400981204      *------------------------------------------------------------------------*
135500990212      * CONFRONTO SELEZIONI FATTE
135600981204      *------------------------------------------------------------------------*
135700990212     C     CONFRO        BEGSR
135800990212     c                   clear                   wconfr
135900990215     c                   clear                   wbol
136000990322     c                   clear                   wbenef
136100990322     c                   clear                   wmitdes
136200990406     c                   eval      wscrivi='N'
136300990215     c                   movel     dctflo        ddct01
136400990212     c
136500990212     c* Selezioni sulle C.A. :
136600990322     c*
136700020821     c* A) Livello (se non Sede)
136800020821    1c                   if        SIMfel <> 0
136900110210    c
137000110210      * controllo anche che la c.a. non abbia dctdt2 > 0
137100110210    2c                   If        dctdt2 > 2
137200110210     c                   eval      wconfr='N'
137300110210     c                   goto      endconfro
137400110210    2c                   EndIf
137500110211    2c                   EndIf
137600110210     c
137700990212     c* 1) Anno CA
137800990216    1c                   if        i43aac>0  and dctaac<>i43aac
137900990212     c                   eval      wconfr='N'
138000990212     c                   goto      endconfro
138100990216    1c                   endif
138200990212     c
138300990212     c* 2) P.O. CA
138400990216    1c                   if        i43fil>0  and dctfil<>i43fil
138500990212     c                   eval      wconfr='N'
138600990212     c                   goto      endconfro
138700990216    1c                   endif
138800990212     c* 3) Tipo anomalia
138900060601     c* Parzializzo tipo anomalia no evento
139000060601    1c                   if        i43tad>*blanks and p1drag<>'E'
139100060601    1c                   if        dcttad<>i43tad
139200990212     c                   eval      wconfr='N'
139300990212     c                   goto      endconfro
139400990216    1c                   endif
139500060601    1c                   endif
139600060601     c
139700060601     c* se evento chain su file eventi per vedere il tipo
139800060601    1c                   if        i43tad>*blanks and p1drag='E'
139900060601     c                   eval      wconfr='N'
140000060601     c                   if        dctnev>0
140100060601     c     kdet          chain     fndet01l                           32
140200060601     c                   if        not *in32  and dettad=i43tad
140300060601     c                   eval      wconfr='S'
140400060601     c                   endif
140500060601     c                   endif
140600060601     c                   if        wconfr='N'
140700060601     c                   goto      endconfro
140800060601    1c                   endif
140900060601    1c                   endif
141000060601     c
141100990212     c* 4) Raggruppamento tipi anomalia
141200990916    1c                   if        i43rgr<>*blanks  OR
141300990916     c                             i43rg2<>*blanks  OR
141400990916     c                             i43rg3<>*blanks
141500990406     c                   eval      wtad=dcttad
141600990406     c                   exsr      DECTAD
141700990219     c
141800990916    2c                   if        §tadragr<>i43rgr AND
141900990916    2c                             §tadragr<>i43rg2 AND
142000990916    2c                             §tadragr<>i43rg3
142100990219     c                   eval      wconfr='N'
142200990219     c                   goto      endconfro
142300990406    2c                   endif
142400990406    1c                   endif
142500990219     c
142600990215     c* 5) P.O. selezione
142700990216    1c                   if        i43psl>0 and i43tsl=' '
142800990215     c     dctlna        lookup    l6                                     31
142900990215     c
143000990215     c                   movel     §dctlnpc      w0030             3 0
143100990215     c     w0030         lookup    l6                                     32
143200990216    2c                   if        not *in31 and not *in32
143300990215     c                   eval      wconfr='N'
143400990215     c                   goto      endconfro
143500990303    2c                   endif
143600990216    1c                   endif
143700990216    1c                   if        i43psl>0 and i43tsl='A'
143800990215     c     dctlna        lookup    l6                                     31
143900990215     c  N31              eval      wconfr='N'
144000990303     c  N31              goto      endconfro
144100990216    1c                   endif
144200990216    1c                   if        i43psl>0 and i43tsl='P'
144300990215     c                   movel     §dctlnpc      w0030             3 0
144400990215     c     w0030         lookup    l6                                     32
144500990215     c  N32              eval      wconfr='N'
144600990303     c  N32              goto      endconfro
144700990216    1c                   endif
144800990215     c* 5) Data C.A.
144900990216    1c                   if        i43dcd>0
145000990216    2c                   if        i43dca=0
145100990215     c                   eval      i43amgdca=i43amgdcd
145200990216    2c                   endif
145300990216    2c                   if        dctdca<i43amgdcd   OR
145400990215     c                             dctdca>i43amgdca
145500990215     c                   eval      wconfr='N'
145600990215     c                   goto      endconfro
145700990216    2c                   endif
145800990216    1c                   endif
145900990218     c
146000990218     c* 6) Data Chiusura C.A.
146100990218    1c                   if        i43dhd>0
146200990218    2c                   if        i43dha=0
146300990218     c                   eval      i43amgdha=i43amgdhd
146400990218    2c                   endif
146500990218    2c                   if        dctdch<i43amgdhd   OR
146600990218     c                             dctdch>i43amgdha
146700990218     c                   eval      wconfr='N'
146800990218     c                   goto      endconfro
146900990218    2c                   endif
147000990218    1c                   endif
147100990218     c
147200990218     c* 7) Causale chiusura C.A.
147300990218    1c                   if        i43cch>*blanks and dctcch<>i43cch
147400990218     c                   eval      wconfr='N'
147500990218     c                   goto      endconfro
147600990218    1c                   endif
147700990218     c* 8) C.A. in Liqu.Trans o Pratica Ass
147800990216    1c                   if        i43tpl<>' ' and dctfpr<>i43tpl
147900990215     c                   eval      wconfr='N'
148000990215     c                   goto      endconfro
148100990216    1c                   endif
148200990218     c* 9) Tipo C.A. : aperte o chiuse
148300990216    1c                   if        i43tca='A' and dctdch>0  OR
148400990215     c                             i43tca='C' and dctdch=0
148500990215     c                   eval      wconfr='N'
148600990215     c                   goto      endconfro
148700990216    1c                   endif
148800990322     c* 10) P.O. responsabile o terminal responsabile
148900990322     c*                            per ordinamento 1- Num C.A.
149000990322    1c                   if        i43ord='1' and i43prs>0  OR
149100990322     c                             i43ord='1' and i43ter>0
149200990215     c                   eval      wconfr='N'
149300990322     c
149400990217     c     kdct2         chain     fndcr02l                           31
149500990216    2c                   dow       not *in31
149600990322   2ac                   if        i43prs>0
149700990322     c
149800990216    3c     dcratb        ifne      ' '
149900990215     c     dcrpor        orne      i43prs
150000990217     c     kdct2         reade     fndcr02l                               31
150100060206   x3c                   else
150200060206     c* Se non immesso, il tipo--> ok
150300060206     c                   if        i43tr1=' ' and i43tr2=' ' and i43tr3=' '
150400990215     c                   eval      wconfr='S'
150500990215     c                   seton                                        31
150600060206     c                   else
150700060206     c                   if        dcrtcr=i43tr1 or dcrtcr=i43tr2 or
150800060206     c                             dcrtcr=i43tr3
150900060206     c                   eval      wconfr='S'
151000060206     c                   seton                                        31
151100060206     c                   else
151200060206     c     kdct2         reade     fndcr02l                               31
151300060206     c                   endif
151400060206     c                   endif
151500060206     c
151600990216    3c                   endif
151700990322   2ac                   endif
151800990322     c
151900990322   2ac                   if        i43ter>0
152000990322    3c     dcratb        ifeq      ' '
152100990322     c     dcrpor        chain     azorg01l                           31
152200070111     c**n31              movel     orgcar        w003a
152300070111     c  n31              movel     orgde0        og140
152400070111     c  n31              movel     §ogara        w003a
152500990322     c   31              movel     *blanks       w003a
152600990322    4c                   if        w003a =i43are
152700060206     c* Se non immesso, il tipo--> ok
152800060206    5c                   if        i43tr1=' ' and i43tr2=' ' and i43tr3=' '
152900990322     c                   eval      wconfr='S'
153000990322     c                   seton                                        31
153100060206   x5c                   else
153200060206    6c                   if        dcrtcr=i43tr1 or dcrtcr=i43tr2 or
153300060206     c                             dcrtcr=i43tr3
153400060206     c                   eval      wconfr='S'
153500060206     c                   seton                                        31
153600060206   x6c                   else
153700060206     c     kdct2         reade     fndcr02l                               31
153800060206    6c                   endif
153900060206    5c                   endif
154000060206     c
154100990322   x4c                   else
154200990322     c     kdct2         reade     fndcr02l                               31
154300990322    4c                   endif
154400990322     c
154500990322   x3c                   else
154600990322     c     kdct2         reade     fndcr02l                               31
154700110210    3c                   endif
154800990322   2ac                   endif
154900990322     c
155000990216    2c                   enddo
155100990322     c
155200990216    2c                   if        wconfr='N'
155300990215     c                   goto      endconfro
155400990216    2c                   endif
155500990216    1c                   endif
155600060206     c* Se leggo per tipo p.o. responsabile devo comunque controllare
155700060206     c*  il tipo
155800060206    1c                   if        i43ord='2' and i43prs>0  OR
155900060206     c                             i43ord='2' and i43ter>0
156000060206     c                   if        i43tr1<>' ' or i43tr2<>' ' or
156100060206     c                             i43tr3<>' '
156200060206    6c                   if        dcrtcr=i43tr1 or dcrtcr=i43tr2 or
156300060206     c                             dcrtcr=i43tr3
156400060206     c                   else
156500060206     c                   eval      wconfr='N'
156600060206     c                   goto      endconfro
156700060206     c                   endif
156800060206     c                   endif
156900060206     c                   endif
157000990322     c
157100110211     c* 11) Peso danneggiato / mancante
157200060125     c                   if        i43pkd>0  and dctpkd<i43pkd
157300060125     c                   eval      wconfr='N'
157400060125     c                   goto      endconfro
157500060125     c                   endif
157600110211     c
157700110211      * se richiesta la stampa x p.o. responsabile non faccio il controllo dei livelli
157800110211     c* A) Livello (se non Sede)
157900110211    1c                   if        SIMfel <> 0
158000110211     c                   select
158100110211    2c                   when      wcontrlnplna='S'
158200110211     c*  Adesso controllo il p.o. utente con la skiera dei p.o. abilitati
158300110211     c                   movel     dctlna        w003a             3
158400110211     c     w003a         lookup    $pog                                   31
158500110211     c  n31§dctlnpc      lookup    $pog                                   31
158600110211     c  n31              eval      wconfr='N'
158700110211     c
158800110211     c* Se devo controllare il terminal, verifico se abilitato
158900110211    2c                   when      wabiter='S'
159000110211     c     §dcrter       lookup    $pog                                   31
159100110211     c  n31              eval      wconfr='N'
159200110211    2c                   endsl
159300110211     c
159400110211    1c                   endif
159500110211     c*
159600110211     c* 12) Beneficiario
159700110211    1c**                 if        i43rsb<>*blanks OR i43ksb>0  or
159800110211     C**                           i43ksb2>0  or i43ksb3>0 or i43ksb4>0  or
159900110211     c**                           i43ksb5>0
160000110211    1c                   if        i43rsb<>*blanks OR $ksb(1)>*zeros
160100110211     c* Devo cercare il beneficario
160200110211     c                   EXSR      CERBOL
160300110211    2c                   if         wbol<>' '
160400110211     c                   exsr      BENEFIC
160500110211    2c                   endif
160600110211     c* - codice
160700110211    2c**                 if        i43ksb>0   AND
160800110211     c**                           p3cksc<>wa43ksb
160900110211     c**                 eval      wconfr='N'
161000110211     c**                 goto      endconfro
161100110211     C
161200110211     c                   if        $ksb(1)>*zeros
161300110211     c                   if        p3cksc<=*zeros
161400110211     c                   eval      wconfr='N'
161500110211     c                   goto      endconfro
161600110211    2c                   endif
161700110211     c
161800110211     c     p3cksc        lookup    $KSB                                   31
161900110211     C                   IF        NOT *IN31
162000110211     c                   eval      wconfr='N'
162100110211     c                   goto      endconfro
162200110211    2c                   endif
162300110211    2c                   endif
162400110211     c
162500110211     c* - ragione sociale
162600110211    2c                   if        i43rsb<>*blanks
162700110211     c                   eval      wrag=%subst(p3dksc:1:wposb)
162800110211    3c                   if        wrag<>i43rsb
162900110211     c                   eval      wconfr='N'
163000110211     c                   goto      endconfro
163100110211    3c                   endif
163200110211    2c                   endif
163300110211    1c                   endif
163400110211     c
163500110211     c* Ricontrollo le abilitazioni  con la filiale del beneficiario
163600110211     c                   if        $ksb(1)>*zeros and wconfr='N'
163700110211     c                   movel     p3cksc        w003a             3
163800110211     c     w003a         lookup    $pog                                   31
163900110211     c                   if        not *in31
164000110211     c                   goto      endconfro
164100110211     c                   else
164200110211     c                   clear                   wconfr
164300110211     c                   endif
164400110211     c                   endif
164500110211     c
164600110211     c* se il controllo delle abilitazioni non è andato a buon fine --> scarto la bolla
164700110211     c                   if        wconfr='N'
164800110211     c                   goto      endconfro
164900110211    1c                   endif
165000110211     c
165100990218     c
165200990215     c*
165300990216    1c                   if        wselbol='S'
165400050711     c* 1) lnp bolla  da DCT
165500050711    3c***                if         i43lnp>0 and i43lnp<>arblnp
165600050711     c                   if         i43lnp>0
165700050711     c                   movel     §dctlnpc      w0030             3 0
165800050711     c                   if         i43lnp<>w0030
165900990215     c                   eval      wconfr='N'
166000990215     c                   goto      endconfro
166100990216    3c                   endif
166200050711    3c                   endif
166300050711     c* 2) lna bolla  da DCT
166400050711    3c                   if        i43lna>0 and i43lna<>dctlna
166500990215     c                   eval      wconfr='N'
166600990215     c                   goto      endconfro
166700990216    3c                   endif
166800050711    c
166900050711     c                   EXSR      CERBOL
167000050711    c
167100050711    2c                   if         wbol<>' '
167200990215     c* 3) Data spedizione
167300990216    3c                   if        i43dsd>0
167400990216    4c                   if        i43dsa=0
167500990215     c                   eval      i43amgdsa=i43amgdsd
167600990216    4c                   endif
167700050711    4c                   if        spedata<i43amgdsd   OR
167800050711     c                             spedata>i43amgdsa
167900990215     c                   eval      wconfr='N'
168000990215     c                   goto      endconfro
168100990216    4c                   endif
168200990218    3c                   endif
168300990322     c
168400990322     c* 4) Mittente
168500990322    1c                   if        i43rsm<>*blanks OR i43ksm>0
168600990322     c                   exsr      MITDES
168700990322     c* - codice
168800990322    2c                   if        i43ksm>0   AND
168900990322     c                             p3cccm<>wa43ksm
169000990322     c                   eval      wconfr='N'
169100990322     c                   goto      endconfro
169200990322    2c                   endif
169300990322     c
169400990322     c* - ragione sociale
169500990322    2c                   if        i43rsm<>*blanks
169600990322     c                   eval      wrag=%subst(p3dccm:1:wposm)
169700990322    3c                   if        wrag<>i43rsm
169800990322     c                   eval      wconfr='N'
169900990322     c                   goto      endconfro
170000990322    3c                   endif
170100990322    2c                   endif
170200990322    1c                   endif
170300990322     c* 5) Destinatario
170400990322    1c                   if        i43rsd<>*blanks OR i43ksd>0
170500990322     c                   exsr      MITDES
170600990322     c* - codice
170700990322    2c                   if        i43ksd>0   AND
170800990322     c                             p3crsd<>wa43ksd
170900990322     c                   eval      wconfr='N'
171000990322     c                   goto      endconfro
171100990322    2c                   endif
171200990322     c
171300990322     c* - ragione sociale
171400990322    2c                   if        i43rsd<>*blanks
171500990322     c                   eval      wrag=%subst(p3drsd:1:wposd)
171600990322    3c                   if        wrag<>i43rsd
171700990322     c                   eval      wconfr='N'
171800990322     c                   goto      endconfro
171900990322    3c                   endif
172000990322    2c                   endif
172100990322    1c                   endif
172200060124     c* 5) Di valore
172300060124     c                   if        i43val='S'   and §ar5bva<>'V'
172400060124     c                   eval      wconfr='N'
172500060124     c                   goto      endconfro
172600060124     c                   endif
172700990215     c
172800990322   x2c                   else
172900990322     c* Se non trovo la bolla le selezioni
173000990216    2c                   endif
173100990216    1c                   endif
173200990406    c
173300990406     c* Se ordinamento 2:
173400990406    1c                   if        i43ord='2'
173500990406     c* 1) escludo gli esclusi dalla reportistica
173600990406    2c                   if        dcttcr='S'
173700990406     c                   eval      wconfr='N'
173800990406     c                   eval      wscrivi='S'
173900990406     c                   goto      endconfro
174000990406    2c                   endif
174100990406     c* 2) escludo le C.A.chiuse con causale da non considerare
174200990406    2c                   if        dctcch<>*blanks
174300990406     c                   eval      wcch=dctcch
174400990406     c                   exsr      DECCCH
174500990406    3c                   if        §cchnre='S'
174600990406     c                   eval      wconfr='N'
174700990406     c                   eval      wscrivi='S'
174800990406     c                   goto      endconfro
174900990406    3c                   endif
175000990406    2c                   endif
175100990406    1c                   endif
175200990407     c* Se il record e' ok --> e' ok anche per scrivere il file
175300990407     c                   eval      wscrivi='S'
175400990215     c*
175500990215     c     endconfro     ENDSR
175600990215      *------------------------------------------------------------------------*
175700990215      * CERCO LA BOLLA DELLA C.A.
175800990215      *------------------------------------------------------------------------*
175900990215     c     CERBOL        BEGSR
176000990322     c* Solo se non l'ho ancora cercata
176100050711    1c                   if        wbol=' '
176200990216     c                   clear                   WPORTO
176300050711     c                   clear                   spedata
176400050711     c                   clear                   spersm
176500050711     c                   clear                   spersd
176600050711     c                   clear                   speccm
176700050711     c                   clear                   speccd
176800050711     c                   clear                   dcknsp
176900990215     c* Cerco la bolla
177000050711    2c                   if        simfel>0
177100040803      * se non sono in sede verifico se CA con legami e se sono in arrivo rispetto la CA
177200050711    3c                   if        §dctlega = 'S'
177300040803     c     Kdct2         chain     fndck01l
177400050711     c
177500050711    4c                   if        not %found(fndck01l)
177600050711     c                   clear                   dckaas
177700050711     c                   clear                   dcklnp
177800050711     c                   clear                   dcknrs
177900050711     c                   clear                   dcknsp
178000050711    4c                   endif
178100050711    3c                   endif
178200050711     c
178300050711     c* Per bolla legata prendo mittente e data spedizione da mamma
178400050711     c*  destinatario da figlia
178500050711    3c                   if        dcknsp>0
178600050711     c     kbolla        chain     fnblp01l
178700050711    5c                   if        %found(FNBLP01L)
178800050711     c                   eval      WBOL='P'
178900050711     c                   exsr      CARDATIBOPAR
179000050711    5c                   endif
179100050711     c
179200050711     c     kbolladck     chain     fnarb01l
179300050711    5c                   if        %found(FNARB01L)
179400050711     c                   eval      WBOL='A'
179500050711     c
179600050711     c* Se non trovato blp della mamma, riempo i campi del mittente e
179700050711     c*  DSP da fnarb della figlia
179800050711    6c                   if        not %found(FNBLP01L)
179900050711     c                   exsr      CARDatibopar
180000050711    6c                   endif
180100050711     c
180200050711     c                   movel     arbrsd        spersd
180300050711    7c                   if        speccd=0 and wporto='A'
180400050711     c                   movel     arbksc        speccd
180500050711    7c                   endif
180600050711     c*
180700050711     c* Imposto numnero spedizione della figlia se:
180800050711     c* 1) non trovato fnblp
180900050711     c* 2) parzializzato per destinatario
181000050711     c* 3) parzializzato per p.o. selezione e lna=p.o. selezione
181100050711     c
181200050711     c                   if        i43psl>0
181300050711     c     dctlna        lookup    l6                                     31
181400050712     c                   else
181500050712     c                   movel     dctlna        w003a
181600050712     c     w003a         lookup    $pog                                   31
181700050711     c                   endif
181800050711     c
181900050711    6c                   if        not %found(FNBLP01L)  or
182000050712     c                             %found(fnarb01l)  and
182100050712     c                             (i43rsd<>*blanks OR i43ksd>0  or  *in31)
182200050711     c                   eval      dctaas = dckaas
182300050711     c                   eval      dctlnp = dcklnp
182400050711     c                   eval      dctnrs = dcknrs
182500050711     c                   eval      dctnsp = dcknsp
182600050711     c                   endif
182700050711     c
182800050711   x5c                   else
182900050711     c*
183000050711     c* se non trovato arb della figlia, riempo i campi del destinatari
183100050711     c*  da fnblp della mamma
183200050711     c                   movel     arbrsd        spersd
183300050711    5c                   endif
183400050711     c
183500050711    3c                   else
183600040803      *
183700050711     c* Se non c'e' legame bolla, cerco prima in fnarb e poi in fnblp
183800050711     c     kbolla        chain     fnarb01l
183900050711    4c                   if        %found(FNARB01L)
184000050711     c                   eval      WBOL='A'
184100050711     c                   else
184200020821     c     kbolla        chain     fnblp01l
184300020821     c                   if        %found(FNBLP01L)
184400020821     c                   eval      WBOL='P'
184500020821     c                   endif
184600050711    4c                   endif
184700990218     c
184800990216     c* Verifico se la bolla e' franco o assegnato
184900050711    4c                   if         wbol<> ' '
185000050711     c                   exsr      CARDAtibopar
185100050712     c                   movel     arbrsd        spersd
185200050711     c
185300050711    5c                   if        wbol='A' and wporto='A'
185400050711     c                   movel     arbksc        speccd
185500050711    5c                   endif
185600050711    4c                   endif
185700050711    3c                   endif
185800990215     c
185900050711   x2c                   else
186000990218     c
186100990218     c* SEDE
186200990809     c     kbolla        chain     TITAS30c                           32
186300990218     c     kbolla        chain     tntmd01l                           32
186400050711    3c                   if        not *in32
186500050711     c                   eval      WBOL='D'
186600050711     c                   movel     tastbl        WPORTO
186700050711     c                   move      arbdsp        spedata
186800050711     c                   movel     arbrsm        spersm
186900050711     c                   movel     arbrsd        spersd
187000050711     c
187100050711    4c                   if        wporto='F'
187200050711     c                   movel     arbksc        speccm
187300050711   x4c                   else
187400050711     c                   movel     arbccm        speccm
187500050711     c                   movel     arbksc        speccd
187600050711    4c                   endif
187700050711    3c                   endif
187800050711    2c                   endif
187900990322     c
188000050711    1c                   endif
188100060124     c
188200060124     c* Vedo se spedizione di valore
188300060125     c                   eval      ar5trd='GEN'
188400060308     c                   if        simfel>0
188500060124     c     k05ar530      chain     fiar501l
188600060124     c                   else
188700060124     c     k05ar530      chain     fiar531c
188800060124     c                   endif
188900060124     c
189000060124     c                   if        %found
189100060124     c                   movel     ar5uni        dar5gen
189200060124     c                   else
189300060124     c                   clear                   dar5gen
189400060124     c                   endif
189500990218     c
189600990215     c                   ENDSR
189700050711      *------------------------------------------------------------------------*
189800050711      * CARICO dati dalla bolla partenza
189900050711      *------------------------------------------------------------------------*
190000050711     c     CARDATiBOPar  BEGSR
190100050711     c                   movel     arbdsp        spedata
190200050711     c                   movel     arbrsm        spersm
190300050711     c
190400050711     c                   exsr      CERTBO
190500050711     c
190600050711    1c                   if        wporto='F'
190700050711     c                   movel     arbksc        speccm
190800050711    1c                   else
190900050711     c                   movel     arbccm        speccm
191000050711     c
191100050711     c* chain in fiar6 per prendere codice di tassazione
191200050711    2c                   if        WBOL='P'
191300050711     c                   eval      Ktrc     = '1'
191400050711     c     kBOLtrc       chain     FIAR6000
191500050711    3c                   if        %found(FIAR601L)
191600050711     c                   move      ar6ksc        w004a
191700050711    4c                   if        w004a<>'9999'
191800050711     c                   movel     ar6ksc        speccd
191900050711    4c                   endif
192000050711    3c                   endif
192100050711    2c                   endif
192200050711    1c                   endif
192300050711     c                   ENDSR
192400050711      *------------------------------------------------------------------------*
192500050711      * CERCO il tipo bolla della spedizione
192600050711      *------------------------------------------------------------------------*
192700050711     c     CERTBO        BEGSR
192800050711     C                   movel     '3A'          kCOD
192900050711     C                   movel(P)  ARBCBO        kKEY
193000050711     C     ktab          CHAIN     TABEL                              31
193100050711     C  n31              movel     TBLUNI        DS3A
193200050711     C  n31              movel     §3ATB1        WPORTO            1
193300050711     c   31              movel     'F'           WPORTO
193400050711     c                   endsr
193500990215      *------------------------------------------------------------------------*
193600990215      * STAMPO LA C.A.
193700990215      *------------------------------------------------------------------------*
193800990215     C     STAMPA        BEGSR
193900000214     c                   clear                   dta4a
194000990310     c*
194100990218     c                   EXSR      ROTTURE
194200990215     c*
194300990215     c* Se non ho ancora chainato la bolla la cerco
194400990215     c                   EXSR      CERBOL
194500990215     c
194600990215     c* Giro le date c.a.
194700000215     c     *iso          move      dctdca        datadmy
194800000215     c     *dmy          move      datadmy       p3dadc
194900990215    1c                   if        dctdch>0
195000000215     c     *iso          move      dctdch        datadmy
195100000215     c     *dmy          move      datadmy       p3dadh
195200990215     c                   else
195300990217     c                   clear                   p3dadh
195400990215    1c                   endif
195500990218     c
195600990215     c* Decodifica tipo anomalia
195700990406     c                   eval      wtad=dcttad
195800990406     c                   exsr      DECTAD
195900990215     c                   movel     §Taddesc      p3dtad
196000990215     c
196100990215     c* Decodifica causale chiusura
196200990218     c                   clear                   p3dcch
196300990215    1c                   if        dctcch<>*blanks
196400990406     c                   eval      wcch=dctcch
196500990406     c                   exsr      DECCCH
196600990215     c                   movel     §cchdesc      p3dcch
196700990215    1c                   endif
196800990218     c
196900990218     C* Tipo calcolo rensponsabilita'
197000990218     c                   clear                   P3CCAR
197100990218     c                   select
197200990218     c                   when      dctcar='M'
197300990218     c                   eval      p3ccar=cosman
197400990218     c                   when      dctcar='A'
197500990218     c                   eval      p3ccar=cosaut
197600990218     c                   other
197700990218     c                   if        dctcar<>*blanks
197800990218     c                   eval      p3ccar=cosric
197900990218     c                   endif
198000990218     c                   endsl
198100990218     c
198200990218     c* Transaz o pratica
198300990218     c                   clear                   p3dtpl
198400990218     c                   if        dctfpr='T'
198500990218     c                   eval      p3dtpl='LqTra'
198600990218     c                   endif
198700990218     c                   if        dctfpr='P'
198800990218     c                   eval      p3dtpl='PrAss'
198900990218     c                   endif
199000020130     c                   if        §dctpfra = 'F'
199100020130     c                   eval      p3dtpl='PrAsF'
199200020130     c                   endif
199300110204     c* decodifico evento  se c'e'
199400110204     c                   clear                   p3deve
199500110204     c                   if        dctnev>0
199600110204     c     kdet          chain     fndet01l                           32
199700110204     c                   if        not *in32
199800110204     c                   eval      wtad=dettad
199900110204     c                   exsr      DECTAD
200000110204     c                   eval      p3deve=dettad+'-'+§taddesc
200100110204     c                   endif
200200110204     c                   endif
200300990215     c
200400050711     c* Data spedizione bolla
200500990629     c                   clear                   p3dADS
200600990216    1c                   if        wbol<>*blanks
200700050711     c     *iso          move      spedata       datadmy
200800000215     c     *dmy          move      datadmy       p3dads
200900990629    1C                   endif
201000000214     c
201100000214     c* Natura Merce
201200000214     c                   movel     dctnas        p3dnas
201300060125     c
201400060125     c* Spedizione di valore
201500060125     c                   if        §ar5bva='V'
201600060125     c                   eval      p3dval='Valore'
201700060125     c                   else
201800060125     c                   clear                   p3dval
201900060125     c                   endif
202000060125     c
202100040726      * Descrizione pezzi danneggiati/mancanti
202200040726     c                   clear                   P3Dpdm
202300040726     c                   eval      *in13   = *off
202400040726if  1c                   if        I43sdm  = 'S'
202500040726     c                   eval      wDCTaac = DCTaac
202600040726     c                   eval      wDCTfil = DCTfil
202700040726     c                   eval      wDCTnca = DCTnca
202800040726     c                   eval      DCStpd  = 'C'
202900040726     c                   movel(p)  DS_keyca      DCSnkt
203000040726     c                   eval      DCSstd  = 'P'
203100040726     c                   clear                   DCSdim
203200040726     c                   clear                   DCShim
203300040726     c                   clear                   DCSnks
203400040726     c                   eval      DCStrc  = 'D'
203500040726     c     k07dcs01      setll     FNDCS000
203600040726     c     k07dcs01      reade     FNDCS000
203700040726do  2c                   dow       not %eof(FNDCS01L)
203800040726if  3c                   if        DCSnot <> *blanks
203900040726if  4c                   if        P3Dpdm  = *blanks
204000040726     c                   eval      P3Dpdm  = DCSnot
204100040726x   4c                   else
204200040726     c                   eval      P3Dpdm  = %trim(P3Dpdm) + ' '
204300040726     c                                     + %trim(DCSnot)
204400040726     c                   leave
204500040726e   4c                   endif
204600040726e   3c                   endif
204700040726     c     k07dcs01      reade     FNDCS000
204800040726e   2c                   enddo
204900040726     c                   eval      *in13  = (P3Dpdm <> *blanks)
205000040726e   1c                   endif
205100060124     c*
205200000214     c* Riferimenti mittente
205300000214     c                   movel     arbrmn        p3drmn
205400000214     c                   if        wbol='P' or wbol='A'
205500000214     c                   movel     arbrma        p3drma
205600000214     c                   endif
205700000214     c* Per la sede si trova in tita4
205800000214     c                   if        wbol='D'
205900000215     c                   eval      ktrc='A'
206000040206     c     kBOLtrc       chain     tita430c                           31
206100000214     c  n31              movel     ta4not        dta4a
206200000214     c                   movel     §ta4arma      p3drma
206300000214     c                   endif
206400990215     c
206500990216     c* Beneficiario
206600990216     c                   EXSR      BENEFIC
206700990216     c
206800990322     c* Mittente e destinatario
206900990322     c                   EXSR      MITDES
207000990322     c
207100000215     c
207200000215     c* Se stampo i DATI di LIQUIDAZIONE, chain su fndcl
207300000215     c                   if        *in09
207400000215     c                   EXSR      STADCL
207500000215     c
207600000215     c                   else
207700000215
207800000215     c* Se stampo i DATI della RESPOSANBILITA, leggo fndcd e fndcr
207900990420     c* Se ci sono della c.a. in attesa di ricalcolo, me lo memorizzo
208000990420     c                   if        dctcar<>'M' and dctcar<>'A' and dctcar<>' '
208100990420     c                   eval      *in06='1'
208200990420     c                   endif
208300990420     c
208400990218     c                   if        i43ord='1'
208500990218     c                   EXSR      STADET1
208600990218     c                   endif
208700990218     c                   if        i43ord='2'
208800990218     c                   EXSR      STADET2
208900990218     c                   endif
209000000215     c                   endif
209100990218     c
209200990215     c                   ENDSR
209300040204      *
209400040204      *---------------------------------------------------------------*
209500040206      * Intabellamento tab. "TAD" = Tipi Anomalia Danni               *
209600040204      *---------------------------------------------------------------*
209700040206     c     Intab_TAD     BEGSR
209800040204      *
209900040204      * Carico i tipi anomalia
210000040204     c                   clear                   ATDd
210100040204     c                   clear                   ATDs
210200040204     c                   clear                   ATDr
210300040206     c                   clear                   TIBS05ds
210400040206     c                   eval      TBI05sif =  KNSIF
210500040206     c                   eval      TBI05cod =  'TAD'
210600040206     c                   call      'TIBS05R'
210700040206     c                   parm                    TIBS05ds
210800040206      *
210900040301if  1c                   if        TBO05err <> 'N'
211000040206      *
211100040204      * - Carico i dati da tab. "TAD"
211200040206     c                   clear                   zz
211300040204     c                   movel     'TAD'         KcodT
211400040206     c                   movel     TBO05sif      Ksif
211500040204     c                   clear                   Klin
211600040206     c     Ktbe2         setll     TNTBE002
211700040206     c     Ktbe2         reade     TNTBE002
211800040204do  2c                   dow       NOT %eof(TNTBE02L)
211900040204if  3c                   if        TBEatb   =  *blanks
212000040204     c                   movel     TBEuni        dTAD
212100040206     c                   add       1             zz
212200040204     c                   movel     TBEke1        ATDd(zz)
212300040204     c                   movel     §TADdesc      ATDs(zz)
212400040204     c                   movel     §TADragr      ATDr(zz)
212500040204e   3c                   endif
212600040206     c     Ktbe2         reade     TNTBE002
212700040204e   2c                   enddo
212800040206      *
212900040206e   1c                   endif
213000040204      *
213100040204     c                   ENDSR
213200040209      *
213300040209      *---------------------------------------------------------------*
213400040209      * Cancellazione record precedentemente estratti dall'utente     *
213500040209      *---------------------------------------------------------------*
213600040209     c     Dlt_WFDDN     BEGSR
213700040209      *
213800040212     c                   open      WFDDN01L
213900040212      *
214000040212     c     KNMUS         chain     WFDDN01
214100040212      *
214200040212do  1c                   dow       %found(WFDDN01L)
214300040212     c     KNMUS         DELETE    WFDDN01
214400040209e   1c                   enddo
214500040212      *
214600040212     c                   close     WFDDN01L
214700040209      *
214800040209     c                   ENDSR
214900040204      *
215000040204      *---------------------------------------------------------------*
215100040204      * Impostazione campi e scrittura record del work-file WFDDN00F  *
215200040204      *---------------------------------------------------------------*
215300040204     c     Wrt_WFDDN     BEGSR
215400040204      *
215500040204      * Se non ho ancora chainato la bolla la cerco
215600040204     c                   exsr      CerBOL
215700040204      *
215800040205      * Decodifico i campi di output
215900040204     c                   exsr      Decod_DDN
216000040204      *
216100040205      * Imposto i campi del record da scrivere
216200040204     c                   exsr      Imp_Rec_DDN
216300040204      *
216400040211      * Reperisco il detaglio colli della C.A. e
216500040205      * Scrivo i records (1 per P.O. anche in minima parte responsabile)
216600040211cas 1c     I43ord        caseq     '1'           Rep_SC1
216700040211cas 1c     I43ord        caseq     '2'           Rep_SC2
216800040211e   1c                   endcs
216900040204      *
217000040204     c                   ENDSR
217100040204      *
217200040204      *---------------------------------------------------------------*
217300040205      * Decodifico i campi "comuni" del record da scrivere            *
217400040204      *---------------------------------------------------------------*
217500040204     c     Decod_DDN     BEGSR
217600040204      *
217700040204      * Reperimento altri dati / decodifiche
217800040204      *
217900040204      * - Fase c.a.
218000040204if  1c                   if        DCTfca <> I05fca
218100040204     c                   clear                   DS_fndfa
218200040204     c                   reset                   FIDN05ds
218300040204     c                   eval      I05fca = DCTfca
218400040204     c                   call      'FIDN05R'
218500040204     c                   parm                    KPJBA
218600040204     c                   parm                    FIDN05ds
218700040204if  2c                   if        O05rec <> *blanks
218800040204     c                   movel     O05rec        DS_fndfa
218900040204e   2c                   endif
219000040204e   1c                   endif
219100040210      *
219200040210      * - Data ultima fase C.A.
219300040210     c     Kdct2         setgt     FNDCF000
219400040210     c     Kdct2         readpe    FNDCF000
219500040210do  1c                   dow       NOT %eof(FNDCF01L)
219600040210     c                             and DCFatb <> *blanks
219700040210     c     Kdct2         readpe    FNDCF000
219800040210e   1c                   enddo
219900040210if  1c                   if            %eof(FNDCF01L)
220000040210     c                             or  DCFatb <> *blanks
220100040210     c                   clear                   DCFdfc
220200040210e   1c                   endif
220300040204      *
220400040204      * - Liquidazione c.a.
220500040204     c     Kdct2         chain     FNDCL000
220600040204if  1c                   if        NOT %found(FNDCL01L)
220700040204     c                             or  DCLatb <> *blanks
220800040204     c                   clear                   DCLipl
220900040204     c                   clear                   DCLvpl
221000040204     c                   clear                   DCLvlp
221100040204     c                   clear                   DCLasn
221200040204     c                   clear                   DCLdea
221300040204e   1c                   endif
221400040204      *
221500040204      * - Causale chiusura
221600040204if  1c                   if        DCTcch <> *blanks
221700040204     c                   eval      Wcch   =  DCTcch
221800040204     c                   exsr      DecCCH
221900040204e   1c                   endif
222000040204      *
222100040204      * - Tipo anomalia
222200040204     c                   eval      Wtad   =  DCTtad
222300040204     c                   exsr      DecTAD
222400040204      *
222500040204      * - Beneficiario generico => decodifico su FNDKA
222600150512     c                   clear                   DKArag
222700040204     c                   move      DCTksc        w0040
222800040204if  1c                   if        w0040  =  8888
222900040204     c                   eval      Ktrc   =  'C'
223000040204     c     KdctK         chain     FNDKA000
223100040204if  2c                   if        NOT %found(FNDKA01L)
223200040205     c                             or  DKAatb <> *blanks
223300040204     c                   clear                   DKArag
223400040204e   2c                   endif
223500040204e   1c                   endif
223600040204      *
223700040204      * Riferimenti mittente
223800040204     c                   clear                   dTA4a
223900040204     c                   eval      Ktrc='A'
224000040206     c     kBOLtrc       chain     TITA430C
224100040204if  1c                   if        %found(TITA430C)
224200040204     c                   movel     TA4not        dTA4a
224300040204e   1c                   endif
224400070927      * Riferimento partner
224500070927     c                   clear                   dsBL4E
224600070927     c                   eval      Ktrc='E'
224700070927     c     kBOLtrc       chain     TITA430C
224800070927if  1c                   if        %found(TITA430C)
224900070927     c                   movel     TA4not        dsbl4E
225000070927e   1c                   endif
225100040204      *
225200040204      * - Bancali
225300040204     c                   clear                   dAR5ban
225400040204     c                   eval      AR5trd = 'BAN'
225500040531     c     K05AR530      chain     FIAR531C
225600040531if  1c                   if            %found(FIAR531C)
225700040204     c                             and AR5atb = *blanks
225800040204     c                   movel     AR5uni        dAR5ban
225900040204     c                   endif
226000040204      *
226100040204     c                   ENDSR
226200040204      *
226300040204      *---------------------------------------------------------------*
226400040205      * Imposto i campi "comuni" del record da scrivere               *
226500040205      * (segnacollo e % responsabilità per P.O.: a dettaglio)         *
226600040204      *---------------------------------------------------------------*
226700040204     c     Imp_Rec_DDN   BEGSR
226800040204      *
226900040209     c                   clear                   WFDDN000
227000040209      *
227100040204      * Impostazione dei campi di output
227200040204      *
227300040204      * - Dati relativi all'estrazione
227400040204     c                   eval      DDNeut   = KNMUS
227500050613     c                   eval      DDNepo   = dutpou
227600040204     c                   eval      DDNedt   = DateU
227700040204      *
227800040204      * - Comunicazione di anomalia
227900040204     c                   eval      DDNaac   = DCTaac
228000040204     c                   eval      DDNfil   = DCTfil
228100040204     c                   eval      DDNnca   = DCTnca
228200040204     c                   eval      DDNdca   = (DCTaac * 10000) + DCTmgc
228300040204      *
228400040204      * - Fase c.a.
228500040210     c                   eval      DDNfca   = DCTfca
228600040204     c                   eval      DDNfcd   = %subst(DFAdes:1:%len(DDNfcd))
228700040210     c                   eval      DDNdfc   = DCFdfc
228800040204      *
228900040204      * - Chiusura c.a.: data e causale
229000040210if  1c                   if        DCTcch  <> *blanks
229100040204     c                   eval      DDNcch   = DCTcch
229200040204     c                   eval      DDNchd   = §CCHdesc
229300040204e   1c                   endif
229400040204      *
229500040204      * - Tipo anomalia danni: codice e descrizione
229600040204     c                   eval      DDNtad   = DCTtad
229700040204if  1c                   if        DCTtad  <> *blanks
229800040204     c                   eval      DDNdta   = §TADdesc
229900040204e   1c                   endif
230000040204      *
230100040204      * - Spedizione
230200040204     c                   eval      DDNaas   = DCTaas
230300040204     c                   eval      DDNlnp   = DCTlnp
230400040204     c                   eval      DDNnrs   = DCTnrs
230500040204     c                   eval      DDNnsp   = DCTnsp
230600040204     c                   eval      DDNdsp   = (ARBaas * 10000) + ARBmgs
230700040204      *
230800040204      * - Riferimento numerico del mittente
230900040204     c                   eval      DDNrmn   = ARBrmn
231000040204      * -      "      alfanumerico del mittente
231100040204     c                   eval      DDNrma   = §TA4Arma
231200040204      * -      "      partner
231300070927     c*********          eval      DDNrpt   = §TA4Arpt
231400070927     c                   movel     §b4ERP        DDNrpt
231500040204      *
231600040205if  1c                   if        Wbol    <> *blanks
231700040206      * - Mittente (da TIBS69R o TITAA30C)
231800040210if  2c                   if        Wporto   = 'F'
231900040210     c                   eval      DDNccm   = ARBksc
232000040210     c                   move      ARBksc        w0040
232100040210x   2c                   else
232200040210     c                   eval      DDNccm   = ARBccm
232300040210     c                   move      ARBccm        w0040
232400040210e   2c                   endif
232500040206sel 2c                   select
232600040206w   2c                   when          Wporto =  'F'
232700040210     c                             and w0040  <> 8888
232800040206     c                             OR  Wporto =  'A'
232900040210     c                             and w0040  <> *zeros
233000040210     c                             and w0040  <> 8888
233100040210     c                             and w0040  <> 9999
233200040206     c                   clear                   TIBS69ds
233300040206     c                   clear                   DS_cnaco
233400040206     c                   clear                   DS_cnind
233500040206     c                   clear                   DS_cnclp
233600040206     c                   clear                   DS_fncls
233700040206     c                   eval      I69kac   = DDNccm
233800040206     c                   eval      I69kin   = DDNccm
233900040206     c                   call      'TIBS69R'
234000040206     c                   parm                    TIBS69ds
234100040206     c                   parm                    DS_cnaco
234200040206     c                   parm                    DS_cnind
234300040206     c                   parm                    DS_cnclp
234400040206     c                   parm                    DS_fncls
234500040206if  3c                   if        O69err   = *blanks
234600040206     c                   movel     ACOrag        DDNrsm
234700040206     c                   movel     INDvia        DDNinm
234800040206     c                   movel     INDcap        DDNcam
234900040206     c                   movel     INDcit        DDNlom
235000040206     c                   movel     INDprv        DDNprm
235100040206     c                   movel     INDsta        DDNnam
235200040206e   3c                   endif
235300040206w   2c                   when           Wporto = 'F'
235400040210     c                             and  w0040  = 8888
235500040206     c                             OR   Wporto = 'A'
235600040210     c                             and (w0040  = *zeros
235700040210     c                              or  w0040  = 8888
235800040210     c                              or  w0040  = 9999)
235900040206     c                   eval      Ktrc     = 'M'
236000040206     c     kBOLtrc       chain     TITAA30C
236100040206if  3c                   if        %found(TITAA30C)
236200040206     c                   eval      DDNrsm   = TAArsc
236300040206     c                   eval      DDNinm   = TAAind
236400040206     c                   eval      DDNcam   = TAAcap
236500040206     c                   eval      DDNlom   = TAAloc
236600040206     c                   eval      DDNprm   = TAAprv
236700040206     c                   eval      DDNnad   = TAAnaz
236800040206e   3c                   endif
236900040206e   2c                   endsl
237000040206      * - Destinatario (da TITAS30C)
237100040210     c                   move      ARBksc        w0040
237200040206if  2c                   if            Wporto =  'A'
237300040206     c                             and (Wbol  =  'A'
237400040206     c                              or  Wbol  =  'D')
237500040210     c                             and w0040  <> 9999
237600040205     c                   eval      DDNccd   = ARBksc
237700040205e   2c                   endif
237800040209     c                   eval      DDNrsd   = TASrsd
237900040209     c                   eval      DDNind   = TASind
238000040209     c                   eval      DDNcad   = TAScad
238100040209     c                   eval      DDNlod   = TASlod
238200040209     c                   eval      DDNprd   = TASprd
238300040209     c                   eval      DDNnad   = TASnzd
238400040205      *
238500040205e   1c                   endif
238600040204      *
238700040204      * - Beneficiario: M=Mittente, D=Destinatario
238800040204     c                   eval      DDNptr   = DCTptr
238900040205      * - Beneficiario alternativo inserito (e già decodificato)
239000040204if  1c                   if        DCTksc   > *zeros
239100040204     c                   eval      DDNksc   = DCTksc
239200040204     c                   eval      DDNdsc   = DKArag
239300040204x   1c                   else
239400040204      * - Beneficiario NON alternativo => uso mitt o dest
239500040204if  2c                   if            Wbol   <> *blanks
239600040204     c                             and DCTptr <> *blanks
239700040204if  3c                   if        DCTptr   = 'M'
239800040204      * - - Per bolla Franco è KSC, per bolla Assegnato è CCM
239900040204sel 4c                   select
240000040204w   4c                   when      Wporto   = 'F'
240100040204     c                   eval      DDNksc   = ARBksc
240200040204w   4c                   when      ARBccm   > 0
240300040204     c                   eval      DDNksc   = ARBccm
240400040204e   4c                   endsl
240500040204     c                   eval      DDNdsc   = ARBrsm
240600040204x   3c                   else
240700040204sel 4c                   select
240800040204      * - - - bolla arrivi o bolla di sede
240900040206w   4c                   when          Wporto =  'A'
241000040206     c                             and (Wbol  =  'A' or Wbol = 'D')
241100040204     c                   move      ARBksc        w0040
241200040204if  5c                   if        w0040   <> 9999
241300040204     c                   eval      DDNksc   = ARBksc
241400040204e   5c                   endif
241500040204      * - - - bolla partenza
241600040206w   4c                   when          Wporto =  'A'
241700040206     c                             and Wbol   =  'P'
241800040206     c                   eval      Ktrc     = '1'
241900040206     c     kBOLtrc       chain     FIAR6000
242000040204     c                   if        NOT %found(FIAR601L)
242100040204     c                   clear                   AR6ksc
242200040204     c                   endif
242300040204     c                   move      AR6ksc        w004a
242400040206if  5c                   if            w004a  >  *zeros
242500040206     c                             and w004a  <> '9999'
242600040204     c                   eval      DDNksc   = AR6ksc
242700040204e   5c                   endif
242800040204e   4c                   endsl
242900040204     c                   eval      DDNdsc   = ARBrsd
243000040204e   3c                   endif
243100040204e   2c                   endif
243200040204e   1c                   endif
243300040205      * - Beneficiario non ancora decodificato
243400040204if  1c                   if            DDNksc <> *zeros
243500040204     c                             and DDNdsc =  *blanks
243600040204     c                   clear                   TIBS69ds
243700040204     c                   eval      I69kac   = DDNksc
243800040204     c                   call      'TIBS69R'
243900040204     c                   parm                    TIBS69ds
244000040204     c                   parm                    DS_cnaco
244100040204     c                   parm                    DS_cnind
244200040204     c                   parm                    DS_cnclp
244300040204     c                   parm                    DS_fncls
244400040204if  2c                   if        O69err   = *blanks
244500040204     c                   eval      DDNdsc   = ACOrag
244600040204e   2c                   endif
244700040204e   1c                   endif
244800040204      *
244900040204      * - Natura merce
245000040204     c                   eval      DDNnas   = DCTnas
245100040204      *
245200040204      * - Numero colli in bolla
245300040204     c                   eval      DDNncl   = ARBncl
245400040204      * - Peso kg in bolla
245500040204     c                   eval      DDNpkf   = ARBpkf
245600040204      * - Numero bancali in bolla
245700040204     c                   eval      DDNnba   = §AR5nba
245800040204      *
245900040204      * - Numero colli danneggiati o mancanti
246000040204     c                   eval      DDNncn   = DCTncn
246100040204      * - Peso kg danneggiati o mancanti
246200040204     c                   eval      DDNpkd   = DCTpkd
246300040204      *
246400040209      * - "Decodifica" pratica assicurativa / liquidazione transattiva
246500040204sel 1c                   select
246600040204w   1c                   when      DCTfpr   = 'T'
246700040206     c                   eval      DDNfpr   = 'LqTra'
246800040204w   1c                   when      §DCTpfra = 'P'
246900040206     c                   eval      DDNfpr   = 'PrAsF'
247000040204w   1c                   when      DCTfpr   = 'P'
247100040206     c                   eval      DDNfpr   = 'PrAss'
247200040204e   1c                   endsl
247300040204      *
247400040209      * - Pratica assicurativa: azienda, anno e numero
247500040204     c                   eval      DDNdit   = DCTdit
247600040204     c                   eval      DDNpra   = DCTpra
247700040204     c                   eval      DDNprn   = DCTprn
247800040209      * - Pratica assicurativa: importo liquidato e divisa
247900040206     c                   eval      DDNipl   = DCLipl
248000040206     c                   eval      DDNvpl   = DCLvpl
248100040209      * - Pratica assicurativa: data valuta pagamento
248200040206     c                   eval      DDNvlp   = DCLvlp
248300040204      *
248400040209      * - Assegno: numero e data
248500040206     c                   eval      DDNasn   = DCLasn
248600040204     c                   eval      DDNdea   = DCLdea
248700040204      *
248800040204     c                   ENDSR
248900040211      *
249000040211      *---------------------------------------------------------------*
249100040211      * Scrivo i records (1 x p.o. responsabile a segnacollo)         *
249200040211      *---------------------------------------------------------------*
249300040211     c     Rep_SC1       BEGSR
249400040211      *
249500040211      * ...Potrei non trovare la responsabilità senza segnacollo
249600040211     c                   eval      wTrovaDet = 'N'
249700040211      *
249800040211      * Reperisco il detaglio colli della C.A.
249900040211     c     Kdct2         setll     FNDCD000
250000040211     c     Kdct2         reade     FNDCD000
250100040211      *
250200040211do  1c                   dow       NOT %eof(FNDCD01L)
250300040211      *
250400040211      * - Seleziono il dettaglio valido e aperto
250500040211if  2c                   if             DCDatb =  *blanks
250600040211     c                             and (DCDdch =  DCTdch
250700040211     c                              or  DCDdch =  *zeros)
250800040211      * - - Imposto dati di output per segnacollo:
250900040211      *     LnA, P.O., nr. serie, nr. segnacollo
251000040211     c                   eval      DDNlna = DCDlna
251100040211     c                   eval      DDNfls = DCDfls
251200040211     c                   eval      DDNnss = DCDnrs
251300040211     c                   eval      DDNnsc = DCDnsc
251400040211      * - - Reperisco le responsabilità e ne scrivo i records nel WF
251500040211     c                   exsr      Rep_Respon
251600040211e   2c                   endif
251700040211      *
251800040211     c     Kdct2         reade     FNDCD000
251900040211e   1c                   enddo
252000040211      *
252100040211      *
252200040211      * SE non ho trovato segnacolli: registro la responsabilità
252300040211      *    senza il segnacollo (come se non ci fosse)
252400040211if  1c                   if        wTrovaDet  =  'N'
252500040211      *
252600040211     c                   eval      DCDlna = DCTlna
252700040211     c                   clear                   DCDfls
252800040211     c                   clear                   DCDnrs
252900040211     c                   clear                   DCDnsc
253000040211     c                   eval      DDNlna = DCTlna
253100040211     c                   clear                   DDNfls
253200050906     c                   clear                   DDNnss
253300040211     c                   clear                   DDNnsc
253400040211     c     Kdct2         setll     FNDCR000
253500040211     c     Kdct2         reade     FNDCR000
253600040211do  2c                   dow       NOT %eof(FNDCR02L)
253700040211     c                             and DCRatb <> *blanks
253800040211     c     Kdct2         reade     FNDCR000
253900040211e   2c                   enddo
254000040211      *
254100040211if  2c                   if        %eof(FNDCR02L)
254200040211      * - Se non esiste nemmeno un record registro casuale "NOCL"
254300040211     c                   eval      DCRflo = '       NOCL    '
254400040211     c                   clear                   DCRpor
254500040211     c                   clear                   DCRper
254600040211     c                   clear                   DCRflo
254700040211     c                   clear                   DCRtcr
254800040211     c                   exsr      Wrt_Rec_DDN
254900040211x   2c                   else
255000040211      * - Altrimenti registro la casuale reperita
255100040211do  3c                   dou       %eof(FNDCR02L)
255200040211if  4c                   if        DCRatb = *blanks
255300040211     c                   exsr      Wrt_Rec_DDN
255400040211e   4c                   endif
255500040211     c     Kdct2         reade     FNDCR000
255600040211e   3c                   enddo
255700040211e   2c                   endif
255800040211      *
255900040211e   1c                   endif
256000040211      *
256100040211     c                   ENDSR
256200040204      *
256300040204      *---------------------------------------------------------------*
256400040206      * Reperimento responsabilità per segnacollo                     *
256500040204      *---------------------------------------------------------------*
256600040205     c     Rep_Respon    BEGSR
256700040204      *
256800040204     c                   movel     DCDnsc        Kseg
256900040206     c     Kdcdr         setll     FNDCR000
257000040206     c     Kdcdr         reade     FNDCR000
257100040204      *
257200040206do  1c                   dow       NOT %eof(FNDCR02L)
257300040204      *
257400040206if  2c                   if        DCRatb    = *blanks
257500040205     c                   exsr      Wrt_Rec_DDN
257600040206e   2c                   endif
257700040205      *
257800040205     c     Kdcdr         reade     FNDCR000
257900040205      *
258000040206e   1c                   enddo
258100040204      *
258200040204     c                   ENDSR
258300040211      *
258400040211      *---------------------------------------------------------------*
258500040211      * Scrivo il record (uno) per il p.o. responsabile in esame      *
258600040211      *---------------------------------------------------------------*
258700040211     c     Rep_SC2       BEGSR
258800040211      *
258900040211      * ...Potrei non trovare la responsabilità senza segnacollo
259000040211     c                   eval      wNOdet   = 'S'
259100040211      *
259200040211      * Controllo se esiste il detaglio colli della C.A.
259300040211     c                   movel     DCRflo        dDCR01
259400040211if  1c                   if        §DCRnsc  >  *zeros
259500040211      *
259600040211      * - Reperisco il detaglio colli della C.A.
259700040211     c                   movel     §DCRnsc       w0070
259800040211     c     kDCRd         chain     FNDCD000
259900040211if  2c                   if            %found(FNDCD01L)
260000040211     c                             and  DCDatb = *blanks
260100040211     c                             and (DCDdch = DCTdch
260200040211     c                              or  DCDdch = *zeros)
260300040211      * - - Imposto dati di output per segnacollo:
260400040211      *     LnA, P.O., nr. serie, nr. segnacollo
260500040211     c                   eval      DDNlna = DCDlna
260600040211     c                   eval      DDNfls = DCDfls
260700040211     c                   eval      DDNnss = DCDnrs
260800040211     c                   eval      DDNnsc = DCDnsc
260900120713     c***                eval      DDNcan = DCDcan
261000120712     c* decodifica
261100120713     c***                if        ddncan<>' '
261200120713     C**+                movel     '3E'          kCOD
261300120713     C***                movel(P)  ddncan        kKEY
261400120713     C***  ktab          CHAIN     TABEL                              31
261500120713     C*+n31              movel     TBLUNI        ds3e
261600120713     c** 31              clear                   ds3e
261700120713     c**                 eval      ddndcan=§3Edes
261800120713     c****               endif
261900120712     c
262000040211      * - - Scrivo direttamente il record (relativo alla
262100040211      *     responsabilità in esame) nel WF
262200040211     c                   eval      wNoDet = 'N'
262300040211     c                   exsr      Wrt_Rec_DDN
262400040211e   2c                   endif
262500040211      *
262600040211e   1c                   endif
262700040211      *
262800040211      * SE non ho trovato segnacolli: registro la responsabilità
262900040211      *    senza il segnacollo (come se non ci fosse)
263000040211if  1c                   if        wNoDet = 'S'
263100040211     c                   eval      DCDlna = DCTlna
263200040211     c                   clear                   DCDfls
263300040211     c                   clear                   DCDnrs
263400040211     c                   clear                   DCDnsc
263500040211     c                   eval      DDNlna = DCTlna
263600040211     c                   clear                   DDNfls
263700050906     c                   clear                   DDNnss
263800040211     c                   clear                   DDNnsc
263900040211     c                   exsr      Wrt_Rec_DDN
264000040211e   1c                   endif
264100040211      *
264200040211     c                   ENDSR
264300040205      *
264400040205      *---------------------------------------------------------------*
264500040205      * Scrittura singolo record dwl workfile WFDDN00F                *
264600040205      *---------------------------------------------------------------*
264700040205     c     Wrt_Rec_DDN   BEGSR
264800040205      *
264900040206      * Decodifica ultimi campi
265000040205      *
265100040206      * - P.O. responsabile del danno e relativa %
265200040206     c                   eval      DDNpor   = DCRpor
265300040206     c                   eval      DDNper   = DCRper
265400040206      *
265500040206      * - Tipo attribuzione reponsabilità
265600040206     c                   movel     DCRflo        dDCR01
265700120713     c                   if        §DCRTER<=*zeros
265800120713     c                   eval      wfil   =dcrpor
265900120713     c                   else
266000120713     c                   eval      wfil=%int(§dcrter)
266100120713     c                   endif
266200120713     c
266300120713     c                   eval      ddnter=wfil
266400120713     c
266500120713     c     wfil          chain     azorg01l                           31
266600120713     c  n31              movel     orgcar        ddnare
266700120713     c   31              movel     *blanks       ddnare
266800120713     c
266900120713     c                   if        ddnare >*zeros
267000120713     C                   movel     '05'          kCOD
267100120713     C                   movel(P)  ddnare        kKEY
267200120713     C     ktab          CHAIN     TABEL                              31
267300120713     C  n31              movel     TBLUNI        ddndare
267400120713     c   31              clear                   ddndare
267500120713     c                   endif
267600120713     c
267700040206     c                   movel     §DCRtre       DDNtre
267800040206     c                   clear                   dTRE
267900040206if  1c                   if        DDNtre  <> *blanks
268000040206     c                   clear                   TIBS02ds
268100040206     c                   eval      T02mod   = 'C'
268200040206     c                   eval      T02sif   = KNSIF
268300040206     c                   eval      T02cod   = 'TRE'
268400040206     c                   movel     DDNtre        T02ke1
268500040206     c                   call      'TIBS02R'
268600040206     c                   parm                    KPJBA
268700040206     c                   parm                    TIBS02ds
268800040206if  2c                   if        T02err  =  *blanks
268900040206     c                   movel     T02uni        dTRE
269000040206e   2c                   endif
269100040206     c                   movel     §TREdesc      DDNtrd
269200040206e   1c                   endif
269300040206      *
269400040206      * - Tabella calcolo responsabilità
269500040206if  1c                   if        DCRtcr  <> *blanks
269600040205     c                   clear                   TIBS02ds
269700040205     c                   eval      T02mod   = 'C'
269800040205     c                   eval      T02sif   = KNSIF
269900040206     c                   eval      T02cod   = 'TCR'
270000040206     c                   movel     DCRtcr        T02ke1
270100040206     c                   call      'TIBS02R'
270200040206     c                   parm                    KPJBA
270300040206     c                   parm                    TIBS02ds
270400040206if  2c                   if        T02err = *BLANKS
270500040206     c                   movel     T02uni        DDNtcr
270600040206e   2c                   endif
270700040206e   1c                   endif
270800040205      *
270900040206     c                   write     WFDDN000
271000040205      *
271100040205     c                   eval      wTrovaDet = 'S'
271200040205      *
271300040205     c                   ENDSR
271400040204      *
271500990406      *------------------------------------------------------------------------*
271600990406      * Decodifico tipo anomalia
271700990406      *------------------------------------------------------------------------*
271800990406     c     DECTAD        begsr
271900990406     c                   if        savtad<>wtad
272000990513     c                   eval      YY=1
272100990513     c     wtad          lookup    atdd(yy)                               32
272200990406      *
272300990513    2C                   IF        *in32
272400990513     C                   movel     atds(yy)      §taddesc
272500990513     C                   movel     atdr(yy)      §tadragr
272600990406     c                   else
272700990406     c                   eval      §taddesc=*all'?'
272800990406     c                   eval      §tadragr=*all'?'
272900990406    2c                   endif
273000990406     c                   eval      savtad=wtad
273100990406    1c                   endif
273200990406     c                   endsr
273300990406      *------------------------------------------------------------------------*
273400990406      * Decodifico causale chiusura
273500990406      *------------------------------------------------------------------------*
273600990406     c     DECCCH        begsr
273700990406    1c                   if        wcch<>savcch
273800990406     C                   clear                   tibs02ds
273900990406     C                   MOVEL     'C'           t02mod
274000990406     C                   MOVEL     knsif         t02sif
274100990406     C                   MOVEL     'CCH'         t02cod
274200990406     C                   MOVEL     wcch          t02ke1
274300990406     C                   CALL      'TIBS02R'
274400990406     C                   PARM                    KPJBA
274500990406     C                   PARM                    TIBS02DS
274600990406      *
274700990406    2C                   IF        t02err = *BLANKS
274800990406     C                   movel     T02uni        DCCH
274900990406     c                   else
275000990406     c                   clear                   dcch
275100990406    2c                   endif
275200990406     c
275300990406     c                   eval      savcch=wcch
275400990406    1c                   endif
275500990406     c                   ENDSR
275600000215      *------------------------------------------------------------------------*
275700000215      * STAMPO DATI DI LIQUIDAZIONE se richiesto per  ordinamento 1
275800000215      *------------------------------------------------------------------------*
275900000215     c     STADCL        BEGSR
276000000215     c                   clear                   WRECSTA
276100000215     c                   clear                   WRECSTAa
276200000215     c                   clear                   WRECSTAd
276300000215     c                   clear                   WRECSTAn
276400000215     c                   clear                   dclipl
276500000215     c                   clear                   dclvpl
276600000215     c                   clear                   p3dvlp
276700000215     c                   clear                   p3ddea
276800000215     c                   clear                   dclasn
276900000215     c                   clear                   p3ddav
277000000215     c
277100000215     c* Addiziono alle bolle stampate
277200000215     c                   eval      wtiptot='S'
277300000215     c                   exsr      CALTOT
277400000215     c
277500000215     c* Leggo il det.colli della C.A. e poi la responsabilita'
277600000215     c     kdct2         chain     fndcl01l                           31
277700000215    1c                   if        not *in31
277800000215     c
277900000215     c* Data valuta pagamento
278000000215    1c                   if        dclvlp>0
278100000215     c     *iso          move      dclvlp        datadmy
278200000215     c     *dmy          move      datadmy       p3dvlp
278300000215    1c                   endif
278400000215     c* Data assegno
278500000215    1c                   if        dcldea>0
278600000215     c     *iso          move      dcldea        datadmy
278700000215     c     *dmy          move      datadmy       p3ddea
278800000215    1c                   endif
278900000215    1c                   endif
279000000215     c
279100000215     c* Cerco la data di avviso danno
279200000215     c                   if        dctprn>0
279300000215     c     kdct2         setgt     fndcf01l                           31
279400000215     c     kdct2         readpe    fndcf01l                               31
279500000215     c                   dow       not *in31 AND dcffca<>545
279600000215     c     kdct2         readpe    fndcf01l                               31
279700000215     c                   enddo
279800000215     c                   if        not *in31 and dcffca=545
279900000215     c     *iso          move      dcfdfc        datadmy
280000000215     c     *dmy          move      datadmy       p3ddav
280100000215     c                   endif
280200000215     c                   endif
280300000215     c
280400000215     c* Stampa testata e dettaglio
280500000215     c                   EXSR      EXCPTOUT
280600000215     c
280700000215     c                   ENDSR
280800990218      *------------------------------------------------------------------------*
280900990406      * STAMPO DETTAGLIO C.A. E REPONSABILITA' PER ordinamento 1
281000990218      *------------------------------------------------------------------------*
281100990218     c     STADET1       BEGSR
281200990218     c                   clear                   WRECSTA
281300990309     c                   clear                   WRECSTAa
281400990309     c                   clear                   WRECSTAd
281500990312     c                   clear                   WRECSTAn
281600990512     c
281700990512     c* Addiziono alle bolle stampate
281800990512     c                   eval      wtiptot='S'
281900990512     c                   exsr      CALTOT
282000990512     c
282100990421     c                   eval      WTROVADET='N'
282200990218     c
282300990218     c* Leggo il det.colli della C.A. e poi la responsabilita'
282400990218     c     kdct2         chain     fndcd01l                           31
282500990421    1c                   if        not *in31
282600990218     c
282700990218    2c                   dow       not *in31
282800990218     c* Dettaglio valido e aperto
282900990218    3c     dcdatb        ifeq      ' '
283000990218     c     dcddch        andeq     dctdch
283100990421    3c     dcdatb        oreq      ' '
283200990421     c     dcddch        andeq     0
283300990218     c* stampo--> leggo la responsabilita' e stampo
283400990218     c                   movel     dcdnsc        kseg
283500990218     c     kdcdr         chain     fndcr02l                           32
283600990218     c
283700990421     c* Posso non trovare la responsabilita' senza segnacollo
283800990421     c                   if        not *in32
283900990218     c
284000990218    4c                   dow       not *in32
284100990218     c                   if        dcratb=' '
284200990421     c                   eval      wtrovadet='S'
284300990218     c* Decodifica campi
284400990218     c                   EXSR      DECODDCR
284500990218     c* Stampa testata e dettaglio
284600990312     c                   EXSR      EXCPTOUT
284700990218     c                   endif
284800990218     c
284900990218     c     kdcdr         reade     fndcr02l                               32
285000990218    4c                   enddo
285100990218     c                   endif
285200990218     c
285300990218    3c                   endif
285400990218     c     kdct2         reade     fndcd01l                               31
285500990218    2c                   enddo
285600990315    1c                   endif
285700990218     c
285800990218     c* Stampo la responsabilita' senza il segnacollo(come se non ci fosse)
285900990218     c                   if        wtrovadet='N'
286000990218     c                   EXSR      STANODETT
286100990218     c                   endif
286200990218     c
286300990218     c                   ENDSR
286400990218      *------------------------------------------------------------------------*
286500990218      * STAMPO DETTAGLIO C.A. E REPONSABILITA' PER ordinamento 2
286600990218      *------------------------------------------------------------------------*
286700990218     c     STADET2       BEGSR
286800100114     c                   clear                   Wnodet
286900990218     c                   if        dctspe<>savspe
287000990218     c                   clear                   WRECSTA
287100990309     c                   clear                   WRECSTAa
287200990309     c                   clear                   WRECSTAd
287300990312     c                   clear                   WRECSTAn
287400990218     c
287500990503     c** Calcolo i totali : spedizioni
287600990503     c                   eval      wtiptot='S'
287700990503     c                   exsr      CALTOT
287800990218     c                   movel     dctspe        savspe
287900990218     c                   endif
288000990218     c
288100990218     c* Se c'e' il segnacollo nella responsabilita' leggo solo
288200990218     c*  il suo dcd altrimenti li leggo tutti
288300040211      * => FALSO !!!
288400040211      * => ...Altrimenti non leggo nulla ed imposto il flag WNODET='S'
288500990218     c                   movel     dcrflo        ddcr01
288600990218     c
288700990218     c                   if        §dcrnsc>*zeros
288800990218     c                   movel     §dcrnsc       w0070             7 0
288900990218     c     kdcrd         chain     fndcd01l                           32
289000990218     c
289100990420     c                   if        not *in32  AND
289200990421     c                             dcddch=dctdch and dcdatb=' '  OR
289300990421     c                             not *in32  AND
289400990421     c                             dcddch=0      and dcdatb=' '
289500990218     c* Decodifica campi
289600990218     c                   EXSR      DECODDCR
289700990218     c* Stampa testata e dettaglio
289800990218     c                   EXSR      EXCPTOUT
289900990420     c
290000990420     c                   else
290100080804     c                   eval      wnodet='S'
290200990218     c                   endif
290300990218     c
290400990218     c                   else
290500990420     c                   eval      wnodet='S'
290600990420     c                   endif
290700990420     c
290800990420     c                   if        wnodet='S'
290900990218     c                   clear                   dcdnsc
291000990218     c                   clear                   dcdfls
291100990218     c                   clear                   dcdnrs
291200990218     c                   eval      dcdlna=dctlna
291300990218     c* Decodifica campi
291400990218     c                   EXSR      DECODDCR
291500990218     c* Stampa testata e dettaglio
291600990218     c                   EXSR      EXCPTOUT
291700990218     c                   endif
291800990218     c
291900990218     c                   ENDSR
292000990503      *------------------------------------------------------------------------*
292100990503      * CALCOLO I TOTALI
292200990503      *------------------------------------------------------------------------*
292300990503     c     CALTOT        BEGSR
292400990503     c* Addiziono 1 alle bolle stampate
292500990503     c                   if        wtiptot='S'
292600990503     c                   add       1             p3ctps
292700990503     c                   else
292800990503     c                   add       dcrper        p3ctpc
292900990503     c                   endif
293000990503     c
293100990503     c* Addiziono  per tipo anomalia
293200990503     c                   eval      zz=1
293300990510     c     dcttad        lookup    ftdd(zz)                               32
293400990503     c
293500990513     c                   if        wtiptot='S'   AND *in32
293600990510     c                   add       1             ftas(zz)
293700990503     c                   else
293800990510     c                   add       dcrper        ftac(zz)
293900990503     c                   endif
294000990503     c
294100000215     c* Addiziono  per codice filtro : solo se richiesti i dati della respon
294200000215     c                   if        not *in09
294300990503     c                   eval      zz=1
294400000323     c                   movel(p)  drrcfl        w003a
294500000323     c     w003a         lookup    fcfl(zz)                               32
294600990503     c
294700000323     c* Se non lo trovo cerco col tipo P/A/T
294800000323     c                   if        not *in32
294900000323     c                   move      dcrtcr        w003a
295000000323     c     w003a         lookup    fcfl(zz)                               32
295100000323     c                   endif
295200000323     c
295300990513     c                   if        wtiptot='S'  AND *in32
295400990510     c                   add       1             ftfs(zz)
295500990503     c                   else
295600990510     c                   add       dcrper        ftfc(zz)
295700990503     c                   endif
295800000215     c                   endif
295900990503     c                   ENDSR
296000990310      *------------------------------------------------------------------------*
296100990310      * STAMPO I TOTALI DI AREA
296200990310      *------------------------------------------------------------------------*
296300990310     c     TOTAREA       BEGSR
296400990310     c                   if        p3ctas>0
296500990310     c                   eval      p3ctps=p3ctas
296600990310     c                   eval      p3ctpc=p3ctac
296700990310     c
296800990310     c                   movel     'A'           WTOT              1
296900990310     c                   clear                   p3dtoan
297000990310     c                   clear                   p3dtopo
297100990310     c                   clear                   p3dtode
297200990310     c                   move      p3care        p3dtopo
297300990310     c                   movel     p3dare        p3dtode
297400990310     c                   eval      p3dtotr='-'
297500990310     c                   exsr      EXCPTOUT
297600990310     c                   movel     ' '           WTOT
297700990310     c
297800990510     c                   EXSR      CALTOTDIS
297900990310     c
298000990310     c                   clear                   p3ctas
298100990310     c                   clear                   p3ctac
298200990310     c                   clear                   p3ctps
298300990310     c                   clear                   p3ctpc
298400990510     c                   clear                   ftas
298500990510     c                   clear                   ftac
298600990510     c                   clear                   ftfs
298700990510     c                   clear                   ftfc
298800990510     c                   clear                   atas
298900990510     c                   clear                   atac
299000990510     c                   clear                   atfs
299100990510     c                   clear                   atfc
299200990310     c                   endif
299300990310     c                   ENDSR
299400990310      *------------------------------------------------------------------------*
299500990310      * STAMPO I TOTALI DI DISTRETTO
299600990310      *------------------------------------------------------------------------*
299700990310     c     TOTDISTR      BEGSR
299800990310     c                   if        p3ctds>0
299900990310     c                   eval      p3ctps=p3ctds
300000990310     c                   eval      p3ctpc=p3ctdc
300100990310     c
300200990310     c                   movel     'D'           WTOT              1
300300990310     c                   clear                   p3dtoan
300400990310     c                   clear                   p3dtopo
300500990310     c                   clear                   p3dtode
300600990310     c                   move      p3cdst        p3dtopo
300700990310     c                   movel     p3ddst        p3dtode
300800990310     c                   eval      p3dtotr='-'
300900990312     c                   eval      wstaare='N'
301000990310     c                   exsr      EXCPTOUT
301100990312     c                   eval      wstaare=' '
301200990310     c                   movel     ' '           WTOT
301300990310     c
301400990310     c                   clear                   p3ctds
301500990310     c                   clear                   p3ctdc
301600990310     c                   clear                   p3ctps
301700990310     c                   clear                   p3ctpc
301800990510     c                   clear                   ftas
301900990510     c                   clear                   ftac
302000990510     c                   clear                   ftfs
302100990510     c                   clear                   ftfc
302200990510     c                   clear                   dtas
302300990510     c                   clear                   dtac
302400990510     c                   clear                   dtfs
302500990510     c                   clear                   dtfc
302600990310     c                   endif
302700990310     c                   ENDSR
302800990218      *------------------------------------------------------------------------*
302900990218      * ROTTURE E SALTI PAGINA
303000990218      *------------------------------------------------------------------------*
303100990218     c     ROTTURE       BEGSR
303200990218     c
303300990512     C* O R D I N A M E N T O 1
303400990218     c                   clear                   dctord1           7
303500990218     c                   movel     dctaac        dctord1
303600990218     c                   move      dctfil        dctord1
303700990218     c
303800990512     c
303900040202if  1c                   if        i43ord='1'  AND
304000990218     c                             dctord1<>savord1
304100990218     c* Al primo giro non stampo i totali
304200040202if  2c                   if        savord1<>*blanks
304300990218     c
304400990218     c* Stampo i campi parziali
304500990218     c                   clear                   p3dtoan
304600990218     c                   clear                   p3dtode
304700990218     c                   clear                   p3dtopo
304800990218     c                   clear                   p3dtotr
304900990218     c
305000990218     c                   movel     savord1       w004a
305100990218     c                   movel     'Anno'        p3dtoan
305200990218     c                   move      w004a         p3dtoan
305300990218     c* P.O.
305400990218     c                   move      savord1       w0030             3 0
305500990218     c     w0030         chain     azorg01l                           32
305600990218     c  N32              movel     orgdes        p3dtode
305700990218     c                   movel     '-'           p3dtotr
305800990218     c                   move      w0030         p3dtopo
305900990218     c                   movel     COSPOAP       p3dtopo
306000990512     c
306100990512     c                   EXSR      STAPO
306200990218     c
306300040202e   2c                   endif
306400040202     c
306500990218     c                   eval      savord1=dctord1
306600990218     c
306700040202e   1c                   endif
306800990218     c
306900990218     c
307000990312     C* O R D I N A M E N T O   2
307100990218     c                   movel     dcrpor        w003a             3
307200040202if  1c                   if        i43ord='2' AND
307300990310     c                             w003a<>savord2
307400990310     c
307500990218     c* Al primo giro non stampo i totali
307600040202if  2c                   if        savord2<>*blanks
307700990218     c
307800990218     c* Stampo i campi parziali
307900990218     c                   clear                   p3dtoan
308000990218     c                   clear                   p3dtode
308100990218     c                   clear                   p3dtopo
308200990218     c                   clear                   p3dtotr
308300990218     c
308400990218     c* P.O.
308500040202if  3c                   if        savord2>*zeros
308600990218     c                   movel     savord2       w0030
308700990218     c     w0030         chain     azorg01l                           32
308800990218     c  N32              movel     orgdes        p3dtode
308900990430     c   32              clear                   orgfl1
309000990218     c                   else
309100990218     c                   movel(p)  COSNES        p3dtode
309200040202e   3c                   endif
309300990218     c
309400990218     c                   movel     '-'           p3dtotr
309500990218     c                   move      w0030         p3dtopo
309600990218     c                   movel     COSPORE       p3dtopo
309700990218     c
309800990512     c                   EXSR      STAPO
309900040202e   2c                   endif
310000990310     c
310100990218     c                   movel     dcrpor        savord2
310200040202e   1c                   endif
310300990218     C                   ENDSR
310400990512      *------------------------------------------------------------------------*
310500990512      * Stampo i totali per P.O.
310600990512      *------------------------------------------------------------------------*
310700050708     c     STAPO         BEGSR
310800990512     c* TOTALI P.O.
310900050708     c                   if        not *in09 and i43toi<>'G'
311000990512     c                   movel     'P'           WTOT
311100990512     c* creo rottura di bolla in quanto c'e' nuova spedizione
311200990512     c                   clear                   savspe
311300990512     c                   EXSR      excptout
311400990512     c                   movel     ' '           WTOT
311500000215     c                   endif
311600000215     c
311700990512     c                   eval      Wnuovopo='S'
311800990512     c
311900990512     c* Addiziono nei totali generali
312000990512     c                   EXSR      CALTOTGEN
312100990512     c* Addiziono nei totali di area
312200990512     c                   EXSR      CALTOTARE
312300990512     c
312400990512     c                   clear                   p3ctpc
312500990512     c                   clear                   p3ctps
312600990512     c                   clear                   ftas
312700990512     c                   clear                   ftac
312800990512     c                   clear                   ftfs
312900990512     c                   clear                   ftfc
313000990512     c                   ENDSR
313100990510      *------------------------------------------------------------------------*
313200990510      * Calcolo totali generali
313300990510      *------------------------------------------------------------------------*
313400990510     c     CALTOTGEN     BEGSR
313500000216     c* Escludo dai totali generali il distretto internazionale
313600000216     c                   if        orgfl3<>'7' OR simfel>0
313700990510     c                   add       p3ctps        p3ctgs
313800990510     c                   add       p3ctpc        p3ctgc
313900990510     c
314000990510     c* totali per filtro
314100990510     c                   eval      zz=1
314200990510     c                   dow       fcfl(zz)<>*blanks
314300990510     c                   if        ftfs(zz)>0
314400990511     c                   eval      xx=1
314500990510     c     fcfl(zz)      lookup    gcfl(xx)                               32
314600990510     c                   add       ftfs(zz)      gtfs(xx)
314700990510     c                   add       ftfc(zz)      gtfc(xx)
314800990510     c                   endif
314900990510     c                   add       1             zz
315000990510     c                   enddo
315100990510     c
315200990510     c* totali per tipo anomalia
315300990510     c                   eval      zz=1
315400990510     c
315500990510    1c                   dow       ftdd(zz)<>'  '
315600990511     c                   eval      xx=1
315700990510     c     ftdd(zz)      lookup    gtdd(xx)                               32
315800990513     c                   if        *in32
315900990510     c                   add       ftas(zz)      gtas(xx)
316000990510     c                   add       ftac(zz)      gtac(xx)
316100990513     c                   endif
316200990513     c                   add       1             zz
316300990510     c                   enddo
316400990510     c
316500990510     c                   else
316600990510     c                   eval      westero='S'
316700990510     c                   endif
316800990510     c                   ENDSR
316900990510      *------------------------------------------------------------------------*
317000990510      * Calcolo totali distretto
317100990510      *------------------------------------------------------------------------*
317200990510     c     CALTOTDIS     BEGSR
317300990510     c                   add       p3ctas        p3ctds
317400990510     c                   add       p3ctac        p3ctdc
317500990510     c* totali per filtro
317600990510     c                   eval      zz=1
317700990510     c                   dow       acfl(zz)<>*blanks
317800990510     c                   if        atfs(zz)>0
317900990511     c                   eval      xx=1
318000990510     c     acfl(zz)      lookup    dcfl(xx)                               32
318100990510     c                   add       atfs(zz)      dtfs(xx)
318200990510     c                   add       atfc(zz)      dtfc(xx)
318300990510     c                   endif
318400990510     c                   add       1             zz
318500990510     c                   enddo
318600990510     c
318700990510     c* totali per tipo anomalia
318800990510     c                   eval      zz=1
318900990510     c
319000990510    1c                   dow       atdd(zz)<>'  '
319100990511     c                   eval      xx=1
319200990510     c     atdd(zz)      lookup    dtdd(xx)                               32
319300990513     c                   if        *in32
319400990510     c                   add       atas(zz)      dtas(xx)
319500990510     c                   add       atac(zz)      dtac(xx)
319600990513     c                   endif
319700990513     c                   add       1             zz
319800990510     c                   enddo
319900990510     c                   ENDSR
320000990510      *------------------------------------------------------------------------*
320100990510      * Calcolo totali area
320200990510      *------------------------------------------------------------------------*
320300990510     c     CALTOTARE     BEGSR
320400990510     c                   add       p3ctps        p3ctas
320500990510     c                   add       p3ctpc        p3ctac
320600990510     c
320700990510     c* totali per filtro
320800990510     c                   eval      zz=1
320900990510     c                   dow       fcfl(zz)<>*blanks
321000990510     c                   if        ftfs(zz)>0
321100990511     c                   eval      xx=1
321200990510     c     fcfl(zz)      lookup    acfl(xx)                               32
321300990510     c                   add       ftfs(zz)      atfs(xx)
321400990510     c                   add       ftfc(zz)      atfc(xx)
321500990510     c                   endif
321600990510     c                   add       1             zz
321700990510     c                   enddo
321800990510     c
321900990510     c* totali per tipo anomalia
322000990510     c                   eval      zz=1
322100990510     c
322200990510    1c                   dow       ftdd(zz)<>'  '
322300990511     c                   eval      xx=1
322400990510     c     ftdd(zz)      lookup    atdd(xx)                               32
322500990513     c                   if        *in32
322600990510     c                   add       ftas(zz)      atas(xx)
322700990510     c                   add       ftac(zz)      atac(xx)
322800990513     c                   endif
322900990510     c                   add       1             zz
323000990510     c                   enddo
323100990510     c
323200990510     c                   ENDSR
323300990310      *------------------------------------------------------------------------*
323400050908      * Salto riga o pagine a cambio p.o.
323500990310      *------------------------------------------------------------------------*
323600990310     c     SALPAG2       begsr
323700990310    3c                   if        i43sap='S'
323800990310     c                   eval      *in02='1'
323900990310     c                   eval      *in03='1'
324000990310     c                   eval      *in04='1'
324100990310   x3c                   else
324200990310     c
324300990310     c* Lascio solo 2 righe vuote
324400990428     c                   if        i43usi='S'
324500050708     c                   if        i43toi=' ' OR
324600050613     c                             (*in01 AND wnuovo='S') or
324700050613     c                             (wnuovo='S' and i43dst<>*blanks) or
324800050613     c                             (wnuovo='S' and i43are<>*blanks)
324900990310     c                   write     dn44z2
325000990428     c                   endif
325100990323     c* Se primo p.o. di area nuova, stampo anche la testata distretto/area
325200990323     c*
325300050613     c                   if        wnuovo='S'
325400050613     c                   if        *in01 or
325500050613     c                             (i43dst<>' ' or i43are<>*blanks)
325600990323     c                   write     dn44t3
325700990323     c                   write     dn44t4
325800990323     c                   write     dn44t3
325900990428     c                   endif
326000050613     c                   endif
326100990428     c                   endif
326200990323     c
326300990310    3c                   endif
326400990323     c
326500990323     c                   clear                   wnuovo
326600990310     c                   ENDSR
326700990216      *------------------------------------------------------------------------*
326800990216      * BENEFICIARIO
326900990216      *------------------------------------------------------------------------*
327000990216     c     BENEFIC       BEGSR
327100990322     c* Solo se non l'ho ancora cercato
327200990322     c                   if        wbenef=' '
327300990322     c                   eval      wbenef='S'
327400990322     c
327500990216     c                   clear                   p3dksc
327600990217     c                   clear                   p3cksc
327700990216     c*
327800990216     c* Beneficiario alternativo inserito
327900990216    1c                   if        dctksc>0
328000990216     c                   movel     dctksc        p3cksc
328100040206     c                   move      p3cksc        w0040
328200990216     c
328300990216     c*Cod.generico --> chain in FNDKA per prendere la ragione sociale
328400990216    2c                   if        w0040=8888
328500990216     c                   eval      ktrc='C'
328600990216     c     kdctk         chain     fndka01l                           31
328700990216    3c                   if        not *in31
328800990216     c                   movel     dkarag        p3dksc
328900990216    3c                   endif
329000990216    2c                   endif
329100990216     c
329200990216   x1c                   else
329300990216     c* non c'e' beneficiario alternativo--> uso mitt o dest
329400990216     c*
329500010706    2c                   if        wbol <> ' ' and dctptr <> ' '
329600990216     c
329700990217    3c                   if        dctptr='M'
329800990216     c* Per bolla franco e' ksc, per bolla assegnato e' ccm
329900050711    4c**                 select
330000050711     c**                 when      wporto='F'
330100050711     c**                 movel     arbksc        p3cksc
330200050711     c**                 when      arbccm>0
330300050711     c**                 movel     arbccm        p3cksc
330400050711     c**                 other
330500050711    4c**                 endsl
330600050711     c**                 movel     arbrsm        p3dksc
330700050711     c
330800050711     c                   movel     speccm        p3cksc
330900050711     c                   movel     spersm        p3dksc
331000990216     c
331100990217   x3c                   else
331200050711    4c**                 select
331300990218     c
331400990218     c* SE bolla arrivi o bolla di sede
331500050711     c**                 when      wporto='A' and wbol='A'  OR
331600050711     c**                           wporto='A' and wbol='D'
331700990218     c
331800050711     c**                 move      arbksc        w0040
331900050711    5c**                 if        w0040=9999
332000050711     c**                 else
332100050711     c**                 movel     arbksc        p3cksc
332200050711    5c**                 endif
332300990218     c* Se bolla Partenza
332400050711    4c**                 when      wporto='A' and wbol='P'
332500990216     c*
332600050711     c**                 eval      Ktrc     = '1'
332700050711     c**   kBOLtrc       chain     fiar601l                           31
332800050711     c** 31              clear                   ar6ksc
332900050711     c**                 move      ar6ksc        w004a             4
333000050711    5c**   w004a         ifgt      *zeros
333100050711     **    w004a         andne     '9999'
333200050711     c*                  movel     ar6ksc        p3cksc
333300050711   x5c**                 else
333400050711    5c**                 endif
333500990216     c
333600050711   x4c**                 other
333700050711    4c**                 endsl
333800050711     c**
333900050711     c**                 movel     arbrsd        p3dksc
334000050711     c
334100050711     c                   move      speccd        w004a             4
334200050711    4c                   if        w004a>*zeros and w004a<>'9999'
334300050711     c                   movel     speccd        p3cksc
334400050711    4c                   endif
334500050711     c                   movel     spersd        p3dksc
334600990217    3c                   endif
334700990216    c
334800990217    2c                   endif
334900990217    1c                   endif
335000990322     c
335100990322     c* Se non e' ancora decodificato lo faccio ora
335200990322    1c                   if        p3dksc=*blanks and p3cksc<>*blanks
335300990322     C                   clear                   TIBS69DS
335400990322     c                   movel     p3cksc        I69kac
335500990322      *
335600990322     C                   CALL      'TIBS69R'
335700990322     C                   PARM                    tibs69DS
335800990322     C                   PARM                    DS_cnaco
335900990322     C                   PARM                    DS_cnind
336000990322     C                   PARM                    DS_cnclp
336100990322     C                   PARM                    DS_fncls
336200990322    2c                   if        o69err=' '
336300990322     c                   movel     acorag        p3dksc
336400990322    2c                   endif
336500990322    1c                   endif
336600990322     c
336700990322    1c                   endif
336800990216     c                   ENDSR
336900990322      *------------------------------------------------------------------------*
337000990322      * CERCO CODICE E RAGIONE SOCIALE MITTENTE E DESTINATARIO
337100990322      *------------------------------------------------------------------------*
337200990322     c     MITDES        BEGSR
337300990322    0c                   if        wmitdes=' '
337400990322     c                   clear                   p3cccm
337500990322     c                   clear                   p3crsd
337600990322     c                   clear                   p3dccm
337700990322     c                   clear                   p3drsd
337800990322     c
337900990322     c                   eval      wmitdes='S'
338000990322     c* Mittente su blp/arb
338100010706    1c                   if        wbol <> *blanks
338200050711     c**                 select
338300050711    2c**                 when      wporto='F'
338400050711     c**                 movel     arbksc        p3cccm
338500050711     c**                 when      arbccm>0
338600050711     c**                 movel     arbccm        p3cccm
338700050711    2c**                 endsl
338800050711     c**                 movel     arbrsm        p3dccm
338900050711     c
339000050711     c                   movel     speccm        p3cccm
339100050712     c                   movel     spersm        p3dccm
339200990322     c* Destinatario
339300050711    2c**                 if        wporto='A' and wbol='A' OR
339400050711    2c**                           wporto='A' and wbol='D'
339500050711     c**                 move      arbksc        w0040
339600050711    3c**                 if        w0040<>9999
339700050711     c**                 movel     arbksc        p3crsd
339800050711    3c**                 endif
339900050711    2c**                 endif
340000050711     c**                 movel     arbrsd        p3drsd
340100050711     c
340200050711     c                   move      speccd        w004a
340300050711     c                   if        w004a>*zeros and w004a<>'9999'
340400050711     c                   movel     speccd        p3crsd
340500050712     c                   endif
340600050712     c                   movel     spersd        p3drsd
340700990322     c
340800990322    1c                   endif
340900990322    0c                   endif
341000990322     c
341100990322    1c                   ENDSR
341200990216      *------------------------------------------------------------------------*
341300990216      * STAMPA C.A. E REPONSABILITA' SENZA DETTAGLIO
341400990216      *------------------------------------------------------------------------*
341500990217     c     STANODETT     BEGSR
341600990216     c* riempo il campo lna con lna della c.a.
341700990216     c                   eval      dcdlna=dctlna
341800990216     c                   clear                   dcdfls
341900990216     c                   clear                   dcdnrs
342000990216     c                   clear                   dcdnsc
342100990216     c*
342200990216     c     kdct2         chain     fndcr02l                           31
342300990421     c                   dow       not *in31  AND
342400990421     c                             dcratb<>' '
342500990421     c     kdct2         reade     fndcr02l                               31
342600990421     c                   enddo
342700990217     c
342800990217     c* Se non esiste nemmeno un record stampo una riga con casuale NOCL
342900990217     c                   if        *in31
343000990217     c                   eval      dcrflo='       NOCL    '
343100990217     c                   clear                   dcrpor
343200990217     c                   clear                   dcrtcr
343300990217     c                   eval      dcrper=dctncn
343400990218     c                   eval      p3ccar=cosaut
343500990217     c
343600990217     c                   exsr      DECODDCR
343700990217     c                   EXSR      EXCPTOUT
343800990217     c
343900990217     c                   else
344000990216     c                   dow       not *in31
344100990216     c                   if        dcratb=' '
344200990216     c                   exsr      DECODDCR
344300990216     c* Stampo
344400990216     c                   EXSR      EXCPTOUT
344500990216     c
344600990216     c                   endif
344700990216     c
344800990216     c     kdct2         reade     fndcr02l                               31
344900990216     c                   enddo
345000990217     c                   endif
345100990216     c
345200990216     c                   ENDSR
345300990216      *------------------------------------------------------------------------*
345400990216      * DECODIFICA DCR
345500990216      *------------------------------------------------------------------------*
345600990216     c     DECODDCR      BEGSR
345700990217     c                   clear                   p3dtre1
345800990217     c                   clear                   p3dtcr
345900990217     c
346000990216     c                   z-add     dcrper        p3dper
346100990216     c                   movel     dcrflo        ddcr01
346200990217
346300990217     c* Tipo attribuzione reponsabilita'--> decodifico
346400990216     c                   movel     §dcrtre       p3ctre
346500990217     c                   if        p3ctre<>*blanks
346600990217     c                   clear                   tibs02ds
346700990217     C                   MOVEL     'C'           t02mod
346800990217     C                   MOVEL     knsif         t02sif
346900990217     C                   MOVEL     'TRE'         t02cod
347000990217     c                   movel     p3ctre        t02ke1
347100990217      *
347200990217     C                   CALL      'TIBS02R'
347300990217     C                   PARM                    KPJBA
347400990217     C                   PARM                    TIBS02DS
347500990217      *
347600990217     C                   IF        T02err = *BLANKS
347700990217     C                   movel     T02uni        DTRE
347800990217     c                   movel     §tredesc      p3dtre1
347900990217     c                   endif
348000990217     c                   endif
348100990217     c
348200990216     c     dcrpor        chain     azorg01l                           32
348300990216     c  N32              movel     orgdes        p3dpor
348400990216     c   32              clear                   p3dpor
348500990217     c
348600990217     c                   if        dcrtcr<>*blanks
348700990217     c                   clear                   tibs02ds
348800990217     C                   MOVEL     'C'           t02mod
348900990217     C                   MOVEL     knsif         t02sif
349000990217     C                   MOVEL     'TCR'         t02cod
349100990217     c                   movel     dcrtcr        t02ke1
349200990217      *
349300990217     C                   CALL      'TIBS02R'
349400990217     C                   PARM                    KPJBA
349500990217     C                   PARM                    TIBS02DS
349600990217      *
349700990217     C                   IF        T02err = *BLANKS
349800990217     C                   movel     T02uni        p3dtcr
349900990217     c                   endif
350000990217     c                   endif
350100990217     c
350200990216     c                   ENDSR
350300990216      *------------------------------------------------------------------------*
350400990216      * STAMPO TESTATA e dettaglio
350500990216      *------------------------------------------------------------------------*
350600990216     C     EXCPTOUT      BEGSR
350700050908     c* Faccio qui la stampa della testata/o salto
350800990310     c*  righe perche' altrimenti mi salta molte righe inutilmente
350900990310     c                   if        wtot=' ' AND
351000990310     c                             wnuovopo='S'
351100990310     c                   exsr      SALPAG2
351200990310     c                   clear                   wnuovopo
351300990310     c                   endif
351400990216     c* Stampo testata
351500990312     c                   EXSR      TESTATA
351600990216     c
351700990309     c                   if        wtot='G'
351800990218     c                   eval      p3dtota=cosGEN
351900990430     c* Solo se ho trovato almeno un p.o. estero, stampo nella riga
352000990430     c*  dei totali generali, "escluso distretto internazionale"
352100990430     c                   if        westero='S'
352200990430     c                   eval      p3dtopo=cosESCL
352300990430     c                   eval      p3dtode=cosINTER
352400990430     c                   eval      *in08='1'
352500990309     c                   endif
352600990430     c                   endif
352700990430     c
352800990309     c                   if        wtot='P'
352900990309     c                   eval      p3dtota=cosPAR
353000990309     c                   endif
353100990310     c                   if        wtot='A'
353200050613     c                   if        simfel=0  or i43ter=0
353300990310     c                   eval      p3dtota=cosAREA
353400990315     c                   else
353500070111     c* Visto che le area possono contenere una parte di terminal
353600070111     c*  non usiamo più la costante TOTALE TERMINAL ma sempre
353700070111     c*  TOTALE AREA
353800070111     c                   eval      p3dtota=cosarea
353900990310     c                   endif
354000990315     c                   endif
354100990310     c                   if        wtot='D'
354200990310     c                   eval      p3dtota=cosDISTR
354300990310     c                   endif
354400990309     c
354500040202if  1c                   if        wtot<>' '
354600990309     c
354700990428     c* USO INTERNO
354800040202if  2c                   if        i43usi='S'
354900050708     c                   if        wtot<>'D' and wtot<>'A' AND
355000050708     c                             i43toi<>'S' AND not *in09
355100990218     c                   write     dn44t3
355200990310     c                   endif
355300000215     c
355400000215     c* se stampo dati di liquidazione, stampo la riga perche' non ho
355500000215     c*  i totali di p.o.
355600050708     c                   if        wtot<>'D' and i43toi<>'S' and *in09
355700000215     c                   write     dn44t3
355800000215     c                   endif
355900000215     c
356000990218     c                   write     dn44z1
356100040202e   2c                   endif
356200990504     c                   eval      *in08='0'
356300990503     c
356400990503     c* Se sto stampando per distretto o area,devo stampare i totali filtr
356500990504     c                   EXSR      FILTRIAREA
356600990504     c                   EXSR      FILTRIDISTR
356700990504     c                   EXSR      FILTRIGEN
356800990503     c
356900990504     c* USO INTERNO
357000990504     c                   if        i43usi='S'
357100990218     c                   write     dn44t3
357200990428     c                   endif
357300990218     c
357400040202x   1c                   else
357500990309     c
357600990428     c* USO INTERNO
357700990428     c                   eval      wstaspe='S'
357800050708if  2c                   if        i43usi='S' and i43toi<>'S'
357900990428     c                   if        wrecsta = ' '
358000990218     c                   write     dn44d03
358100990216     c                   movel     '1'           WRECSTA
358200990216     c                   else
358300990218     c                   write     dn44d04
358400990216     c                   endif
358500040202e   2c                   endif
358600990218     c* Addiziono i colli
358700000215     c                   if        not *in09
358800990503     c                   eval      wtiptot='C'
358900990503     c                   exsr      CALTOT
359000990309     c                   endif
359100040202e   1c                   endif
359200990309     c
359300990216     c                   ENDSR
359400990504      *------------------------------------------------------------------------*
359500990504      *  FILTRI dei totali di area
359600990504      *------------------------------------------------------------------------*
359700990504     C     FILTRIAREA    BEGSR
359800990504     c                   if        wtot='A'
359900990504     c                   movea     atas          wtas
360000990504     c                   movea     atac          wtac
360100990504     c                   movea     atfs          wtfs
360200990504     c                   movea     atfc          wtfc
360300990504     c                   EXSR      STAFILTRI
360400990504     c                   endif
360500990504     c                   ENDSR
360600990504      *------------------------------------------------------------------------*
360700990504      *  FILTRI dei totali generali
360800990504      *------------------------------------------------------------------------*
360900990504     C     FILTRIGEN     BEGSR
361000990504     c                   if        wtot='G'
361100990504     c                   movea     gtas          wtas
361200990504     c                   movea     gtac          wtac
361300990504     c                   movea     gtfs          wtfs
361400990504     c                   movea     gtfc          wtfc
361500990504     c                   EXSR      STAFILTRI
361600990504     c                   endif
361700990504     c                   ENDSR
361800990504      *------------------------------------------------------------------------*
361900990504      *  FILTRI dei totali di distretto
362000990504      *------------------------------------------------------------------------*
362100990504     C     FILTRIDISTR   BEGSR
362200990504     c                   if        wtot='D'
362300990504     c                   movea     dtas          wtas
362400990504     c                   movea     dtac          wtac
362500990504     c                   movea     dtfs          wtfs
362600990504     c                   movea     dtfc          wtfc
362700990504     c                   EXSR      STAFILTRI
362800990504     c                   endif
362900990504     c                   ENDSR
363000990503      *------------------------------------------------------------------------*
363100990503      * STAMPO i TOTALI FILTRI
363200990503      *------------------------------------------------------------------------*
363300990503     c     STAFILTRI     BEGSR
363400990503     c* Prima i filtri
363500990513     c* solo per ordinamento 2
363600990513     c                   if        i43ord='2'
363700990513     c                   EXSR      LASCIO1RG
363800990513     c
363900990503     c                   eval      p4des1=cosFILTRO
364000990503     c                   eval      zz=1
364100990503     c
364200990503    1c                   dow       wcfl(zz)<>'  '
364300990510    2c                   if        wtfs(zz)>0
364400990503     c                   movea     wcfl(zz)      w38(1)
364500990503     c                   movea     '-'           w38(3)
364600990503     c
364700990503     C                   clear                   tibs02ds
364800990503     C                   MOVEL     'C'           t02mod
364900990503     C                   MOVEL     knsif         t02sif
365000990503     C                   MOVEL     'RRD'         t02cod
365100990503     C                   MOVEL     'A1'          t02ke1
365200000323     C                   MOVEL     wcfl(zz)      w002a             2
365300000323     C                   MOVEL     w002a         t02ke2
365400990503     C                   CALL      'TIBS02R'
365500990503     C                   PARM                    KPJBA
365600990503     C                   PARM                    TIBS02DS
365700990503      *
365800990510    3C                   IF        t02err = *BLANKS
365900990503     c                   movel     t02uni        w035a            35
366000990503     c                   else
366100990503     c                   eval      w035a=*all'?'
366200990510    3c                   endif
366300990503     c
366400990503     c                   movea     w035a         w38(4)
366500990503     c                   movea     w38           p4des2
366600000328     c
366700000328     C                   MOVE      wcfl(zz)      w001a             1
366800000328     c                   clear                   p4des3
366900000328     c                   if        w001a='A'
367000000328     c                   movel     'Arrivo'      p4des3
367100000328     c                   endif
367200000328     c                   if        w001a='P'
367300000328     c                   movel     'Partenza'    p4des3
367400000328     c                   endif
367500000328     c                   if        w001a='T'
367600000328     c                   movel     'Transito'    p4des3
367700000328     c                   endif
367800000328     c
367900990503     c                   eval      p4ctps=wtfs(zz)
368000990503     c                   eval      p4ctpc=wtfc(zz)
368100990503     c
368200990504     C* Stampo
368300990511     c                   if        wtot<>'N'
368400990503     c                   exsr      TESTATA
368500990511     c                   endif
368600990511     c
368700990504     c                   exsr      STAMPOFIL
368800990504     c                   clear                   p4des1
368900990510    "c                   ENDIF
369000990504     c
369100990503     c                   add       1             zz
369200990510    1c                   enddo
369300990513     c                   endif
369400990503     c
369500990503     c* Poi i tipi anomalia
369600990513     c                   EXSR      LASCIO1RG
369700990513     c
369800990503     c                   eval      p4des1=cosTIPANOM
369900990503     c                   eval      zz=1
370000990503     c
370100990513    1c                   dow       wtdd(zz)<>'  '
370200990513     c                   if        wtas(zz)>0
370300990513     c                   movea     wtdd(zz)      w38(1)
370400990503     c                   movea     '-'           w38(3)
370500990503     c
370600990503     c                   eval      wtad=wtdd(zz)
370700990503     c                   exsr      DECTAD
370800990503     c                   movel(p)  §taddesc      w035a
370900990503     c
371000990503     c                   movea     w035a         w38(4)
371100990503     c                   movea     w38           p4des2
371200990503     c                   eval      p4ctps=wtas(zz)
371300990503     c                   eval      p4ctpc=wtac(zz)
371400990503     c
371500990504     c* Stampo
371600990503     c                   exsr      TESTATA
371700990504     c                   exsr      STAMPOFIL
371800990504     c                   clear                   p4des1
371900990513     c                   endif
372000990503     c
372100990503     c                   add       1             zz
372200990503    1c                   enddo
372300990503     c
372400990503     c                   ENDSR
372500990504      *------------------------------------------------------------------------*
372600990504      * LASCIO 1 RIGA
372700990504      *------------------------------------------------------------------------*
372800990504     c     LASCIO1RG     BEGSR
372900990504     c* USO INTERNO
373000990504     c                   if        i43usi='S'
373100990504     c                   write     dn44z5
373200990504     c                   endif
373300990504     c                   ENDSR
373400990504      *------------------------------------------------------------------------*
373500990504      * STAMPO 1 RIGA DI TOTALE FILTRO
373600990504      *------------------------------------------------------------------------*
373700990504     c     STAMPOFIL     BEGSR
373800990504     c* USO INTERNO
373900990504     c                   if        i43usi='S'
374000990504     c                   write     dn44z4
374100990504     c                   endif
374200990511     c
374300990504     c                   ENDSR
374400990312      *------------------------------------------------------------------------*
374500990312      * STAMPO TESTATA
374600990312      *------------------------------------------------------------------------*
374700990312     c     TESTATA       BEGSR
374800990428     c* stampo solo se sto stampando i totali o se devo anche
374900990428     c*  stampare il dettaglio
375000990428     c* USO INTERNO
375100990428    1C                   if        i43usi='S'
375200050708    2C                   if        wtot<>' ' OR i43toi<>'S'
375300990428    3C                   if        CURLINE>60   OR *IN02
375400990312     c                   write     dn44t1
375500990312     c                   write     dn44ts
375600050708     c                   if        i43toi<>'S'
375700990312     c                   write     dn44t2
375800990428     c                   endif
375900990312     c                   write     dn44t3
376000050613     c                   if        *in01 or i43dst<>*blanks or i43are<>*blanks
376100050613     c                   write     dn44t4
376200050613     c                   write     dn44t3
376300050908     c
376400050908     c                   if        wtot=' '
376500050908     c                   clear                   wnuovo
376600050613     c                   endif
376700050908     c                   endif
376800050908     c
376900990312     c                   clear                   wrecsta
377000990312     c                   eval      *in02='0'
377100990428    3c                   endif
377200990428    2c                   endif
377300990428    1c                   endif
377400990312     c
377500990312     c                   ENDSR
377600990406      *------------------------------------------------------------------------*
377700990406      * SCRIVO DATI PER REPORT CONSUNTIVO
377800990406      *------------------------------------------------------------------------*
377900990406     c     SCRIREP       BEGSR
378000990406     c                   clear                   ktae
378100990406     c                   clear                   kfpr
378200990408     c                   clear                   kcrs
378300990406     c                   clear                   kcch
378400990917     c                   clear                   kcrc
378500090319     c* Terminal dal dcrflo
378600090319     c                   eval      ddcr01=dcrflo
378700090319     c                   if        dcrtcr='P'
378800090319     c                   eval      kttf='P'
378900090319     c                   else
379000090319     c                   eval      kttf='A'
379100090319     c                   endif
379200110801     c
379300110801     c* Verifico se scrivere anche file clienti particolari
379400110801     c                   exsr      cerbol
379500110801     c* Verifico se cod cliente mittente è tra quelli particolari
379600121126     c*  e solo AVARIE e solo per il terminal previsto
379700121126     c*  o Destinatario o Beneficiario
379800110801     c                   clear                   Indx
379900130909     c                   clear                   kter_p
380000121126    1c                   if        speccm>0
380100121126     c                   eval      Indx=%lookup(speccm:ksc_ex)
380200121126    1c                   endif
380300121126    1c                   if        Indx=0 and speccd>0
380400121126     c                   eval      Indx=%lookup(speccd:ksc_ex)
380500121126    1c                   endif
380600121126    1c                   if        Indx=0
380700121126     c                   exsr      BENEFIC
380800121126    2c                   if        p3cksc>*zeros
380900121126     c                   eval      Wben_n=%int(p3cksc)
381000121126     c                   eval      Indx=%lookup(wben_n:ksc_ex)
381100121126    2c                   endif
381200121126    1c                   endif
381300121126     c
381400121126    1c                   if        Indx>0
381500121126     c                   eval      wtad=dcttad
381600121126     c                   exsr      DECTAD
381700121126    2c                   if        §tadragr<>'V'
381800121126     c                   clear                   Indx
381900121126    2c                   endif
382000121126    1c                   endif
382100121126     c
382200121126     c* Se si tratta di avarie controllo il terminal
382300121126    1c                   if        Indx>0
382400121126     c                   select
382500121126     c*  cliente momentaneamente bloccato
382600130909    2c                   when      ksc_are(indx)=888
382700121126     c                   clear                   Indx
382800121126     c* abilitato a tutto:  memorizzo per qualsiasi terminal
382900130909    2c                   when      ksc_are(indx)=999
383000130909     c                   eval      kter_p= ksc_tr(indx)
383100121126   x2c                   other
383200121126     c
383300130909     c* Abilito solo i terminal della C.A. se previsto dall'area del term di riferimento della tab
383400130909     c     kter          chain     azorg01l
383500130909    3c                   if        %found(azorg01l)
383600121126     c
383700130909    4c                   if        orgcar<> ksc_are(indx)
383800121126     c                   clear                   indx
383900130909   x4c                   else
384000130909     c                   eval      kter_p=ksc_tr(indx)
384100130909    4c                   endif
384200130909
384300130909   x3c                   else
384400130909     c                   clear                   indx
384500130909    3c                   endif
384600121126    c
384700121126    2c                   endsl
384800121126    1c                   endif
384900121126     c*
385000090319     c
385100990406     c* decodifico tipo anomalia e causale chiusura
385200990408     c                   eval      wtad=dcttad
385300990406     c                   exsr      DECTAD
385400990408     c
385500990406     c                   if        dctcch<>*blanks
385600990406     c                   eval      wcch=dctcch
385700990406     c                   exsr      DECCCH
385800990406     c* se da non memorizzare sul reporto la causale chiusura, come se fosse
385900990406     c*  aperta
386000990406     c                   if        §cchmerp='S'
386100990406     c                   eval      kcch=dctcch
386200990406     c                   endif
386300990406     c                   endif
386400990406     c
386500990406     c* Prendo mese di elaborazione
386600990406     c                   movel     §tadragr      wragr
386700990406     c
386800990406     c* se manca Tipo gestione, la devo desumere   T / P
386900990406     c                   if        dctfpr<>*blanks
387000990406     c                   movel     dctfpr        kfpr
387100990406     c                   else
387200990407     c                   EXSR      DESUMI
387300990406     c                   endif
387400990406     c
387500990406     c* Se c'e' evento devo prendere il suo tipo anomalia
387600990406     c                   if        dctnev>0
387700990406     c     kdet          chain     fndet01l                           32
387800990406     c                   if        not *in32
387900990406     c                   eval      ktae=dettad
388000990406     c                   endif
388100990406     c                   endif
388200990408     c
388300990408     c                   if        dcttcr='S'  OR
388400990408     c                             dctcch<>*blanks and §cchnre='S'
388500990408     c                   eval      kcrs='N'
388600990408     c                   else
388700990408     c                   eval      kcrs='S'
388800990408     c                   endif
388900990917     c* riserva
389000990917     c                   eval      kcrc=dctcrc
389100990408     c
389200090319     c     krdn          chain     wfrdn51l                           32
389300110801     c
389400990406     c                   if        *in32
389500990406     c                   clear                   wfrdn000
389600121126     c                   eval      tipocli=' '
389700110801     c                   exsr      RiempiRDN
389800110801     c                   endif
389900110801     c
390000990406     c                   add       dcrper        rdnncn
390100990406     c   32              write     wfrdn000
390200990406     c  N32              update    wfrdn000
390300110801     c
390400110801     c* Scrivo file clienti particolari
390500110801     c                   if        Indx>0
390600110912     c     krdnAV        chain     wfrdn51l                           32
390700110801     c
390800110801     c                   if        *in32
390900110912     c                   clear                   wfrdn000
391000121126     c
391100121127     c                   eval      tipocli='P'
391200110801     c                   exsr      RiempiRDN
391300121126     c                   eval      tipocli=' '
391400121126     c
391500110912     c* Cambia solo il tipo elaborazione da G/R a A/B
391600110912     c                   eval      rdntie=ktieAV
391700110801     c                   endif
391800110801     c
391900110801     c                   add       dcrper        rdnncn
392000110912     c   32              write     wfrdn000
392100110912     c  N32              update    wfrdn000
392200110801     c                   endif
392300990406     c
392400990406     c                   ENDSR
392500110801      *------------------------------------------------------------------------*
392600110801      * Riempimento campi chiave file WFRDN
392700110801      *------------------------------------------------------------------------*
392800110801     c     RiempiRDN     BEGSR
392900110801     c                   eval      rdndal=wdal
393000110801     c                   eval      rdnall=wal
393100110801     c                   eval      rdndiv=p3cdst
393200110801     C                   movel     '17'          kCOD
393300110801     C                   movel(P)  rdndiv        kKEY
393400110801     C     ktab          CHAIN     TABEL                              31
393500110801     C  n31              movel     TBLUNI        rdnddiv
393600110801     c
393700110801     c                   eval      rdnare=p3care
393800110801     C                   movel     '05'          kCOD
393900110801     C                   movel(P)  rdnare        kKEY
394000110801     C     ktab          CHAIN     TABEL                              31
394100110801     C  n31              movel     TBLUNI        rdndare
394200110801     c
394300110801     c                   eval      rdnttf=kttf
394400121126     c
394500121126     c* Per i clienti particolari non imposto la filiale
394600121126     c                   if        tipocli=' '
394700110801     c                   eval      rdnfil=dcrpor
394800110801     c     rdnfil        chain     azorg01l
394900110801     c                   if        %found(azorg01l)
395000110801     c                   eval      rdndfil=orgdes
395100110801     c                   endif
395200121126     c                   else
395300121126     c                   clear                   rdnfil
395400121126     c                   clear                   rdndfil
395500121126     c                   endif
395600110801     c
395700130909     c* per i clienti particolari uso kter_p
395800130909     c                   if        tipocli=' '
395900110801     c                   eval      rdnter=kter
396000130909     c                   else
396100130909     c                   eval      rdnter=kter_p
396200130909     c                   endif
396300110801     c     rdnter        chain     azorg01l
396400110801     c                   if        %found(azorg01l)
396500110801     c                   eval      rdndter=orgdes
396600110801     c                   endif
396700110801     c
396800110801     c                   eval      rdntae=ktae
396900110801     c                   eval      rdnrga=wragr
397000110801     c                   eval      rdntad=dcttad
397100110801     c                   eval      rdncch=kcch
397200110801     c                   eval      rdnfpr=kfpr
397300110801     c                   eval      rdncrs=kcrs
397400110801     c                   eval      rdncrc=kcrc
397500110801     c                   eval      rdntie=ktie
397600110801     c                   ENDSR
397700990407      *------------------------------------------------------------------------*
397800990407      * Desumo tipo gestione se manca
397900990407      *------------------------------------------------------------------------*
398000990407     C     DESUMI        BEGSR
398100990407     c* Se c'e' numero evento --> "P"
398200990407    1c                   if        dctnev>0
398300990408     c                   eval      kfpr='P'
398400990407    1c                   endif
398500990407     c
398600990407     c* Se c'e' imp. da assicurare -->  "P"
398700990408    1c                   if        kfpr=' '
398800990408     c                   EXSR      CERBOL
398900990408    2c                   if        arbias>0
399000990408     c                   eval      kfpr='P'
399100990407    2c                   endif
399200990407    1c                   endif
399300990407     c
399400990407     c                   movel     dctflo        ddct01
399500990407     c
399600990407     c* Se e' franco import -->  "P"
399700990408    1c                   if        kfpr=' '  AND
399800990407     c                             §dcttisp='I' and §dctport='F'
399900990408     c                   eval      kfpr='P'
400000990407    1c                   endif
400100020422     c
400200020513     c* Se e' franco export DPD con responsabilità partner -->  "P"
400300020422    1c                   if        kfpr     = ' '  and
400400020513     c                             §dcteuro = 'D'  and
400500020513     c                             §dcttisp = 'E'  and
400600020422     c                             §dctport = 'F'  and
400700020422     c                             DCTres   = 'P'
400800020422     c                   eval      kfpr='P'
400900020422    1c                   endif
401000990407     c
401100990407     c* Se c'e' Mandato -->  "P"
401200990408     c                   if        kfpr=' '
401300990407     c* se si tratta di cambio di porto il mandato non c'e'
401400990407     c                   EXSR      CERBOL
401500110503     c**                 if        arbcca<>'9'
401600110503     c                   if        §DCTcca<>'9'
401700990407     c                   move      arbksc        w0040
401800990407     c                   if        w0040<>8888 and w0040<>9999
401900990407     c* Richiamo pgm che mi dice se c'e' mandato
402000990407     c                   clear                   fidn48ds
402100990407     c                   eval      i48ksc=arbksc
402200990407     c                   eval      i48ctr=arbctr
402300990407     c                   eval      i48tsp=arbtsp
402400050711     c                   eval      i48dsp=spedata
402500990408     c                   eval      i48por=§dctport
402600990407     c                   if        §dcttisp=' '
402700000126     c* Carico tariffe italia + dpd
402800000126     c                   eval      i48fie='J'
402900990407     c                   else
403000000202     c                   eval      i48fie='S'
403100990407     c                   endif
403200990408     c                   eval      i48mns='S'
403300990407     c*
403400990407     c                   call      'FIDN48R'
403500990407     c                   parm                    kpjba
403600990407     c                   parm                    fidn48ds
403700990408     c
403800010417     c                   if        not *in32 and *in32
403900010417     c                   except    DETT
404000010417     c                   endif
404100990408     c
404200990407     c                   if        o48mand='S'
404300990408     c                   eval      kfpr='P'
404400990407     c                   endif
404500990407     c
404600990407     c                   endif
404700990407     c                   endif
404800990407     c                   endif
404900990407     c
405000990408     c                   if        kfpr=' '  AND
405100990407     c                             dctipv>0 and dctipv<§STDllt
405200990408     c                   eval      kfpr='T'
405300990407     c                   endif
405400990407     c
405500990408     c                   if        kfpr=' '
405600990407      * Determino se limite risarcibile da utilizzare è "CMR" o "Legge 450"
405700990407     c                   EXSR      IMPlimris
405800990407     c                   if        (dctpkd * wlimris) > §STDllt
405900990408     c                   eval      kfpr='P'
406000990407     c                   else
406100990408     c                   eval      kfpr='T'
406200990407     c                   endif
406300990407     c                   endif
406400990407     c
406500990407     c                   ENDSR
406600990407      *****************************************************************
406700990407      *  DETERMINO LIMITE RISARCIBILE DA UTILIZZARE
406800990407      *****************************************************************
406900990407     C     IMPLIMRIS     BEGSR
407000990407      *
407100010413      * se spedizione non nazionale o EXPORT DPD
407200010413      *
407300020422     c                   IF            (§DCTeuro =  'X'
407400020422     c                              or  §DCTeuro =  'P')
407500020422     c                             and  §DCTtisp <> *blanks
407600020422     c                             or   §DCTeuro =  'D'
407700020422     c                             and  §DCTtisp =  'E'
407800010413      * controvalore CMR
407900010413     c                   z-add     §geccm        Wlimris
408000990407      *
408100990407      *    LEGGE 450
408200990407     c                   ELSE
408300010413     c                   z-add     §GElrp        wlimris
408400990407     c                   ENDIF
408500990407      *
408600990407     C                   ENDSR
408700990212      *------------------------------------------------------------------------*
408800990212      * ROUTINE INIZIALE
408900990212      *------------------------------------------------------------------------*
409000990212     C     *INZSR        BEGSR
409100981204      *
409200981204     C     *ENTRY        PLIST
409300981204     C                   PARM                    KPJBA
409400990212     c                   movel     kpjbu         fidn43ds
409500050610      *
409600050610     c     *dtaara       define    §azute        azuteds
409700050610     c     *dtaara       define    §datiute      ddatiute
409800050610     c                   in(E)     *dtaara
409900050610    1c                   If        %error  or RSUT = *blanks
410000050610     c                   Clear                   Tibs34ds
410100050610     c                   Call      'TIBS34R'
410200050610     c                   Parm                    Tibs34ds
410300050610     c                   In        *dtaara
410400050610    1c                   EndIf
410500050610
410600050610     c                   Clear                   wabi
410700050610     c                   Clear                   dLat
410800050610
410900050610      * Verifica errori e autorità profilo
411000050610s   1c                   Select
411100050610      * se ho errori nei dati utente esco dal pgm
411200050610w   1c                   When      DutErr = 'E'
411300050610     c                   GoTo      Fine
411400050610      * se non c'è l'abilitazione
411500050610      * --> se 1° livello, abilitazioni al terminal
411600050610      *     se 2° livello, abilitazioni al punto operativo
411700050610      *     se sede è impossibile ma se capita mando a fine il pgm
411800050610w   1c                   When      UteAut = *Blanks
411900050610if  2c                   If        DutLpo = '1'
412000050610     c                   Eval      wabi   = 'TP'
412100050610e   2c                   EndIf
412200050610if  2c                   If        DutLpo = '2'
412300050610     c                   Eval      wabi   = 'PO'
412400050610e   2c                   EndIf
412500050610if  2c                   If        DutLpo = 'S'
412600050610     c                   GoTo      Fine
412700050610e   2c                   EndIf
412800050610      * carica le abilitazioni del profilo
412900050610x   1c                   Other
413000050610     c                   Movel     UteFaf        Dute01
413100050610     c                   If        §UteEca <> *Blanks
413200050610     c                   Eval      wabi = §UteEca
413300050610     c                   Else
413400050610     c                   Eval      wabi = UteAut
413500050610     c                   EndIf
413600050610e   1c                   EndSl
413700050610
413800050610      * controllo se ok l'abilitazione dell'utente
413900050610     c                   Clear                   Tibs02ds
414000050610     c                   Eval      T02Mod = 'C'
414100050610     c                   Eval      T02Sif = knsif
414200050610     c                   Eval      T02Cod = 'LAT'
414300050610     c                   Movel(p)  wabi          T02Ke1
414400050610     c                   Call      'TIBS02R'
414500050610     c                   Parm                    kpjba
414600050610     c                   Parm                    Tibs02ds
414700050610if  1c                   If        T02Err = *Blanks
414800050610     c                   Eval      dLat = T02Uni
414900050610e   1c                   EndIf
415000050610      * errore o non abilitato (impossibile xchè controllato nel filtro)
415100050610if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
415200050610     c                   goto      fine
415300050610   x1c                   Else
415400050610      * Reperimento dei P.O. gestibili dall'utente
415500050610     c                   clear                   TRUL31DS
415600050610     c                   clear                   TRUL31DS2
415700050610     c                   eval      I31abi = wabi
415800050610     c                   eval      I31cdi = DUTdis
415900050610     c                   eval      I31car = DUTare
416000050610     c                   eval      I31cpo = DUTpou
416100050610     c                   call      'TRUL31R'
416200050610     c                   parm                    KPJBA
416300050610     c                   parm                    TRUL31DS
416400050610     c                   parm                    TRUL31DS2
416500050610if  2c                   if        O31pog > *zeros
416600050610     c                   movea     O31pog        $pog
416700050610     c                   movea     O31arg        $arg
416800050610     c                   movea     O31dig        $dig
416900050610x   2c                   else
417000050610     c* impossibile perchè controllato nel filtrp
417100050610     c                   goto      fine
417200050610e   2c                   endif
417300050610    1c                   endif
417400050610     c
417500981204      *
417600050610     C                   Z-ADD     1             CODUT
417700981204
417800981204      * reperisco data e ora
417900981204     C                   TIME                    W0140
418000981204      * UDATE IN GGMMAAAA
418100981204     C                   MOVE      W0140         WDTGIO
418200990217     C                   MOVE      W0140         WDATE
418300981209     C                   MOVEL     W0140         WORA
418400981204      * UDATE IN AAAAMMGG
418500981204     C                   Z-ADD     WDTGIO        G02DAT
418600981204     C                   MOVEL     *BLANK        G02ERR
418700981204     C                   CALL      'XSRDA8'
418800981204     C                   PARM                    WLBDAT
418900981204     C                   MOVEL     G02INV        DATEU
419000120713     c* udate e 1 mese
419100120713     c     *iso          movel     dateu         dataiso
419200120713     c                   subdur    1:*m          dataiso
419300120713     c                   movel     dataiso       meno1mes          8 0
419400981204      *
419500990215     c* Apertura file diversa tra sede e filiale
419600050613     c**                 clear                   L1user
419700050613     c**                 clear                   L6user
419800990215     c     simfel        ifgt      0
419900990215     c                   open      fnarb01l
420000990215     c                   open      fnblp01l
420100990215     c                   open      fnblt01l
420200990215     c                   open      fnart01l
420300991116     c                   open      fiar601l
420400060124     c                   open      fiar501l
420500990216     c     *loval        setll     fnart01l
420600990216     c     *loval        setll     fnblt01l
420700020821     c
420800990215     c                   else
420900990809     c                   open      TITAS30c
421000000214     c                   open      TITA430c
421100990218     c                   open      tntmd01l
421200090319     c                   open      wfrdn51l
421300060124     c                   open      FIAR531C
421400050906     c                   if           I43cfd = 'S'
421500050906     c                             or I43cfd = 'P'
421600040206     c                   open      TITAA30C
421700050906if  5c                   if        I43cfd = 'P'
421800040212     c                   exsr      Dlt_WFDDN
421900050906e   5c                   endif
422000120713
422100120713     c* pulisco sempre e comunque per vecchiaia
422200120713     C/EXEC SQL
422300120713     c+   delete from  wfddn00f where ddnedt< :meno1mes
422400120713     C/end-EXEC
422500120713     c
422600040206     c                   open      WFDDN00F
422700040206     c                   endif
422800990809     c     *loval        setll     TITAS30c
422900990216     c                   seton                                        01
423000990215     c                   endif
423100990407     c
423200010413      ***
423300010413      * leggo tabella variabili tariffe per limite risarcibile  nella moneta corrente
423400010413      ***
423500010413      *
423600010413      * recupero la moneta corrente
423700010413     C                   CLEAR                   tibs02ds
423800010413      *
423900010413     C                   MOVEL     'L'           T02TLA
424000010413     C                   MOVEL     'C'           T02MOD
424100010413     C                   MOVEL     KNSIF         T02SIF
424200010413     C                   MOVEL     'GED'         T02COD
424300010907     C                   MOVEL     'DANNI'       T02KE1
424400010413      *
424500010413     C                   CALL      'TIBS02R'
424600010413     C                   PARM                    KPJBA
424700010413     C                   PARM                    tibs02ds
424800010413      *
424900010910     c                   movel     T02UNI        DGEDDN
425000010413      * recupero i valori nella moneta corrente
425100010413     C                   CLEAR                   tibs02ds
425200010413      *
425300010413     C                   MOVEL     'L'           T02TLA
425400010413     C                   MOVEL     'C'           T02MOD
425500010413     C                   MOVEL     KNSIF         T02SIF
425600010413     C                   MOVEL     'GEI'         T02COD
425700010910     C                   MOVEL     §GEDDBA       T02KE1
425800010413      *
425900010413     C                   CALL      'TIBS02R'
426000010413     C                   PARM                    KPJBA
426100010413     C                   PARM                    tibs02ds
426200010413      *
426300010413     c                   movel     t02uni        DGEI
426400990408      ***
426500990408      * leggo tabella standards danni
426600990408      ***
426700990408     c                   clear                   TIBS02DS
426800990408     C                   MOVEL     'C'           t02mod
426900990408     C                   MOVEL     knsif         t02sif
427000990408     C                   MOVEL     'STD'         t02cod
427100990408     C                   MOVEL(P)  1             t02ke1
427200010910     C                   MOVEL(P)  §gedDBA       t02ke2
427300990408      *
427400990408     C                   CALL      'TIBS02R'
427500990408     C                   PARM                    KPJBA
427600990408     C                   PARM                    TIBS02DS
427700990408     c
427800990408     C                   if        t02err = *BLANKS
427900990408     C                   MOVEL     T02UNI        DSTD
428000990408     c                   else
428100990408     c                   clear                   dstd
428200990408     C                   endif
428300110801     c*
428400110801     c* Carico tabella clienti particolari
428500121127     c*  solo se scrivo il report
428600130909    1c                   if        i43crp='S'
428700121127     c
428800121127     C                   Z-ADD     0             K                 4 0
428900110801     C                   MOVEL     'CXQ'         TBecod
429000110801     C     tbecod        SETLL     tntbe01l
429100110801     C                   DO        *HIVAL
429200110801     C     tbecod        READE     tntbe01l
429300110801     c                   if        %eof(tntbe01l)
429400110801     c                   leave
429500110801     c                   endif
429600110801
429700110801     C     tbeatb        IFeq      ' '
429800110801     C                   ADD       1             K
429900110801     C                   MOVEL     TBeKE2        ksc_ex(k)
430000121126     c                   movel     tbeuni        dcxq
430100130909
430200130909     c* Terminal di riferimento in cui memorizzare i dati
430300130909    3c                   if        §cxqterm>*zeros
430400130909     c                   movel     §cxqterm      ksc_tr(k)
430500130909     c* Area
430600130909    4c                   if        §cxqarea='T'
430700130909     c                   movel     999           ksc_are(k)
430800130909   x4c                   else
430900130909     c
431000130909     c     ksc_tr(k)     chain     azorg01l
431100130909    5c                   if        %found(azorg01l)
431200130909     c                   movel     orgcar        ksc_are(k)
431300130909   x5c                   else
431400130909     c                   movel     888           ksc_tr(K)
431500130909     c                   movel     888           ksc_are(k)
431600130909    5c                   endif
431700130909    4c                   endif
431800130909    c
431900130909   x3c                   else
432000130909     c* non riesco a memorizzare --> importo 888
432100130909     c                   movel     888           ksc_tr(k)
432200130909     c                   movel     888           ksc_are(k)
432300130909    3c                   endif
432400130909     c
432500110801     C                   ENDIF
432600110801     C                   ENDDO
432700121127     c
432800121127     c* Controllo riempimento schiera
432900121127     c                   movel     kpjbu         savkpjbu
433000121127     c                   clear                   trul0sds
433100121127     c                   eval      i0stla='L'
433200121127     c                   eval      i0sski='KSC_EX'
433300121127     c                   eval      i0sele=%elem(ksc_ex)
433400121127     c                   eval      i0spie=K
433500121127     c                   eval      i0sfile='Cienti tab CXQ'
433600121127     c                   eval      i0ssif=knsif
433700121127     c                   eval      i0spgm='FIDN47R'
433800121127     c                   movel     trul0sds      kpjbu
433900121127     c                   call      'TRUL0SR'
434000121127     c                   parm                    kpjba
434100121127
434200121127     c                   movel     savkpjbu      kpjbu
434300121127     c
434400121127     C                   ENDIF
434500981204
434600981204     C*----------------------------------------------------*
434700981204     C* KEY   KEY    KEY   KEY   KEY   KEY   KEY   KEY  KEY
434800981204     C*----------------------------------------------------*
434900981204      *
435000981204     c* ACCESSO tntbe
435100990406     C     ktbe          KLIST                                                  *
435200990217     C                   KFLD                    kcodt                          *
435300981204     C                   KFLD                    kke1                           *
435400981204     C                   KFLD                    kke2                           *
435500981204     C                   KFLD                    klin                           *
435600981204     C                   KFLD                    ksif                           *
435700990512     C     ktbe2         KLIST                                                  *
435800990512     C                   KFLD                    kcodt                          *
435900990512     C                   KFLD                    klin                           *
436000990512     C                   KFLD                    ksif                           *
436100090320     C     kcae          KLIST                                                  *
436200090320     C                   KFLD                    kepa                           *
436300090323     C                   KFLD                    ktfpa                          *
436400981204      *
436500981204     c* ACCESSO fndct
436600990215     C     kdct1         KLIST                                                  *
436700990219     C                   KFLD                    kfil                           *
436800990215     C                   KFLD                    i43aac                         *
436900981204      *
437000981204     c* ACCESSO fndcd
437100990215     C     kdct2         KLIST                                                  *
437200990215     C                   KFLD                    dctaac                         *
437300981210     C                   KFLD                    dctfil                         *
437400981210     C                   KFLD                    dctnca                         *
437500990216     C     kdcdr         KLIST                                                  *
437600990216     C                   KFLD                    dctaac                         *
437700990216     C                   KFLD                    dctfil                         *
437800990216     C                   KFLD                    dctnca                         *
437900990216     C                   KFLD                    KSEG                           *
438000990215     c* ACCESSO fndcd
438100990216     C     kdctk         KLIST                                                  *
438200990215     C                   KFLD                    dctaac                         *
438300990215     C                   KFLD                    dctfil                         *
438400990215     C                   KFLD                    dctnca                         *
438500990215     C                   KFLD                    KTRC                           *
438600981204      *
438700991116     c* ACCESSO FNARB/FNBLP
438800990216     C     kbolla        KLIST                                                  *
438900981209     C                   KFLD                    dctaas                         *
439000981209     C                   KFLD                    dctlnp                         *
439100981209     C                   KFLD                    dctnrs                         *
439200981204     C                   KFLD                    dctnsp                         *
439300050711     C     kbolladck     KLIST                                                  *
439400050711     C                   KFLD                    dckaas                         *
439500050711     C                   KFLD                    dcklnp                         *
439600050711     C                   KFLD                    dcknrs                         *
439700050711     C                   KFLD                    dcknsp                         *
439800040726      * Accesso FNDCS01L
439900040726     c     k07dcs01      klist
440000040726     c                   kfld                    DCStpd
440100040726     c                   kfld                    DCSnkt
440200040726     c                   kfld                    DCSstd
440300040726     c                   kfld                    DCSdim
440400040726     c                   kfld                    DCShim
440500040726     c                   kfld                    DCSnks
440600040726     c                   kfld                    DCStrc
440700040206      * Accesso TITA430C / TITAA30C / FIAR601L
440800040206     c     kBOLtrc       klist
440900040206     c                   kfld                    DCTaas
441000040206     c                   kfld                    DCTlnp
441100040206     c                   kfld                    DCTnrs
441200040206     c                   kfld                    DCTnsp
441300040206     c                   kfld                    Ktrc
441400040531      * Accesso FIAR531C
441500040206     c     K05AR530      klist
441600040206     c                   kfld                    DCTaas
441700040206     c                   kfld                    DCTlnp
441800040206     c                   kfld                    DCTnrs
441900040206     c                   kfld                    DCTnsp
442000040206     c                   kfld                    AR5trd
442100981210     c* ACCESSO TABEL
442200981210     C     ktab          KLIST                                                  *
442300040210     C                   KFLD                    CODUT                          *
442400990216     C                   KFLD                    kcod                           *
442500981210     C                   KFLD                    kkey                           *
442600010907     c*
442700990503     C     kDCR3         KLIST                                                  *
442800090323     C                   KFLD                    kteralfa                       *
442900090323     C                   KFLD                    kfil                           *
443000090323     C                   KFLD                    drrtre                         *
443100090323     C     kDCRcer       KLIST                                                  *
443200090323     C                   KFLD                    kteralfa                       *
443300090323     C                   KFLD                    kfil                           *
443400990218     C     kDCRT         KLIST                                                  *
443500990218     C                   KFLD                    dcraac                         *
443600990218     C                   KFLD                    dcrfil                         *
443700990218     C                   KFLD                    dcrnca                         *
443800990218     C     kdcrd         KLIST                                                  *
443900990218     C                   KFLD                    dctaac                         *
444000990218     C                   KFLD                    dctfil                         *
444100990218     C                   KFLD                    dctnca                         *
444200990218     C                   KFLD                    w0070                          *
444300990219     C     korg2         KLIST                                                  *
444400990219     C                   KFLD                    i43dst                         *
444500070111     C**                 KFLD                    kcar                           *
444600070111     C                   KFLD                    i43are                         *
444700990406     C     kRDN          KLIST                                                  *
444800090319     C                   KFLD                    ktie                           *
444900090319     C                   KFLD                    wdal                           *
445000090319     C                   KFLD                    wal                            *
445100990406     C                   KFLD                    p3cdst                         *
445200990406     C                   KFLD                    p3care                         *
445300090319     C                   KFLD                    kter                           *
445400090319     C                   KFLD                    kttf                           *
445500090319     C                   KFLD                    dcrpor                         *
445600990406     C                   KFLD                    ktae                           *
445700990406     C                   KFLD                    dcttad                         *
445800990917     C                   KFLD                    kcrc                           *
445900990917     C                   KFLD                    kcch                           *
446000990408     C                   KFLD                    kcrs                           *
446100990408     C                   KFLD                    kfpr                           *
446200110912     C     kRDNav        KLIST                                                  *
446300110912     C                   KFLD                    ktieav                         *
446400110912     C                   KFLD                    wdal                           *
446500110912     C                   KFLD                    wal                            *
446600110912     C                   KFLD                    p3cdst                         *
446700110912     C                   KFLD                    p3care                         *
446800130909     C                   KFLD                    kter_p                         *
446900110912     C                   KFLD                    kttf                           *
447000121126     C                   KFLD                    kfil_0                         *
447100110912     C                   KFLD                    ktae                           *
447200110912     C                   KFLD                    dcttad                         *
447300110912     C                   KFLD                    kcrc                           *
447400110912     C                   KFLD                    kcch                           *
447500110912     C                   KFLD                    kcrs                           *
447600110912     C                   KFLD                    kfpr                           *
447700990406     C     KDET          KLIST                                                  *
447800990406     C                   KFLD                    dctaae                         *
447900990406     C                   KFLD                    dctnev                         *
448000981209     c*
448100981204     C                   ENDSR
448200990219     c*
448300990408     OQsysprt   E            dett
448400990408     O                       dctaac
448500990408     O                       dctfil        z   +  1
448600990408     O                       dctnca        z   +  1
448700990408     O                       dctaas        z   +  1
448800990408     O                       dctlnp        z   +  1
448900990408     O                       dctnrs        z   +  1
449000990408     O                       dctnsp        z   +  1
449100990408     O                       i48ksc        z   +  1
449200990408     O                       i48ctr        z   +  1
449300990408     O                       i48por            +  1
449400990408     O                       i48fie            +  1
449500990408     O                       i48tsp            +  1
449600990408     O                       o48mand           +  1
449700990408     O                       o48FTC            +  1
449800990408     O                       kfpr              +  1
449900990324** TES - testata
450000990324ELENCO  COMUNICAZIONI  DI  ANOMALIA
450100990324   ELENCO  C.A.  DA  ARCHIVIARE
450200121219** OVRPRT
450300121219OVRPRTF FILE(FIDN44P) HOLD(*YES)
