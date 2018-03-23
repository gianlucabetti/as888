000100121212     H DEBUG DECEDIT('0,') DATEDIT(*YMD.) option(*nodebugio)
000200060718     H*------------------------------------------------------------------------*
000201121127     Ftntbe01l  uf   e           k Disk
000400121206     Ftivrb01l  uf   e           k Disk
000401121207     Ftitas30c  if   e           k Disk    prefix(f_)
000402121207     Ftitaa30c  if   e           k Disk
000403121211     Ftita430c  if   e           k Disk    prefix(f_)
000700060721     FTabel00f  if   e           k Disk
000800060714     Fazorg01l  if   e           k Disk
000801121206     Ffiar531c  if   e           k disk
000802121206     fTitaI01L  if   e           k disk
000803140127     F***fnevb04l  if   e           k disk
000900121207     fTitas00f  o    e             Disk    rename(Titas000:Titas0)
001100121127     fTita400f  o    e             Disk    rename(Tita4000:Tita40)
001101121210     ffiar500f  o    e             Disk    rename(fiar5000:fiar50)
001102121221     fwfrpf00F  o    e             Disk    usropn
001103121221     fwfrpf00S  o    e             Disk    rename(wfrpf000:wfrpfs)
001104121221     f                                     usropn
001300060714      *------------------------------------------------------------------------*
001400060714      *   S C H I E R E
001500060714      *------------------------------------------------------------------------*
001600121211     d camposare       ds           255
001700121211     d  sare                   1    255    DIM(85)                              Codici Area
002000121102     d
002100060714      *------------------------------------------------------------------------*
002200060714      *   V A R I A B I L I
002300060714      *------------------------------------------------------------------------*
002400060714
002401121218     D w_simul         s              1                                         flag simulazione
002402130110     D w_area          s              1
002403130325     D w_tuttea        s              1
002404130110     D w_contnum       s              7  0 inz(5000000)
002405121218     D k               s              5  0                                      indice
002406121212     D pos             s              3  0                                      indice
002407121212     D rec_ok          s              1
002408121127     d Kcode           S                   like(tbecod)
002409121127     d Kke1            S                   like(tbeke1)
002600121207     d ktrc            s                   like(TaaTrc)
002601121211     d kAr5Trd         s                   Like(Ar5Trd)
002602140127     D*Wcev            s                   like(evbcev) inz('RIC')
002603121211     d savdtas         s                   Like(dtas)
002604121211     d savdtasv        s                   Like(dtasv)
002605121211     d antepo          s                   Like(tasimv)
002606121211     d s_§ta4arma      s                   Like(§ta4arma)
002607121212     d wTara           s                   Like(§QtTpc)
002608121212     d wNtara          s                   Like(TasPkf)
002609121212     d rem             s              5  0
002610121212     d risult          s              5  0
004200061006
004300121127     d dataiso_cor     s               d   datfmt(*iso)
004301121127     d gg              s              2  0
004302121127     d woggi           s              8  0
004303121127     d dataiso         s               d   datfmt(*iso)
004400061006     d dataeur         s               d   datfmt(*eur)
004401121206     d filksc          s              3  0
004402140507     d wnoRA           s                   like(ida06no1)
004403121217     d wflg            s                   like(vrbflg)
004404121221     d s_acorag        s                   like(acorag)
004500060717
004600060714      *------------------------------------------------------------------------*
004700060714      *   D S   I N T E R N E / E S T E R N E
004800060714      *------------------------------------------------------------------------*
004900060717
008600060714
008700060714     d Azuteds       e ds                  extname(Azute00f)
008800060714     d dDatiute      e ds
008900060714     d ds_cnaco      e ds                  extname(Cnaco00f)
009000060714     d ds_cnind      e ds                  extname(Cnind00f)
009100060714     d ds_cnclp      e ds                  extname(Cnclp00f)
009200060714     d ds_fncls      e ds                  extname(Fncls00f)
009201121210     d ds_vrb        e ds                  extname(tivrb00f)
009202121210     d ds_vrbOr      e ds                  extname(tivrb00f)  prefix(Or_)
010000060714     d Kpjba         e ds
010100060714     d Tibs34ds      e ds
010200060714     d Tibs69ds      e ds
010201121207     d fnlv55ds      e ds
010300060714     d Trul34ds      e ds
010400060714     d  f_i34tla     e                     extfld(i34tla)
010500060714     d  f_o34err     e                     extfld(o34err)
010600060714     d  f_o34msg     e                     extfld(o34msg)
010601140507     d fidna6ds      e ds
010602121211     d tibs02ds      e ds
010603121211
010604121211     d drpfd         e ds
010605121213     d ds3a_sav      e ds                  extname(ds3a) prefix(s_)
010606150330     d ds3a          e ds                  extname(ds3a)
010607121221     d ds17          e ds
010608121221     d ds05          e ds
010609150108     d ds7R          e ds
010610121128
010611121128      ** DS Calcolo tassazione
010612121206     d dtas          e ds                  prefix(dtas_)
010613121206     d dtasv         e ds                  prefix(dtasv_)
010614121128     d  sv                     1     20    dim(20)
010615121128     d  va                    21    140P 3 dim(20)
010616121206     d dar5ban       e ds
010617121206     d dar5bnb       e ds
010618151026     d dar5fat       e ds
010619160108     d dar5gen       e ds
010620121207     d dstb          e ds
010621121211     d dtasflo       e ds
010622121211     d dtas01        e ds
010623121211     d dta4a         e ds
010624121212     d dsqt1         e ds
010625121213      * Costanti per note R.A.
010626121213     d wcvolu          c                   Const('VOLUME +')
010627121213     d wcpeso          c                   Const('PESO   +')
010628121213     d wcnzd           c                   Const('NAZ.DESTIN. Da:')
010629121213     d wccad           c                   Const('CAP DESTIN. Da:')
010630121213     d wccts           c                   Const('COD.TASSAZ. Da:')
010631121213     d wcffdt          c                   Const('FERMO DEPOSITO Tolto')
010632121213     d wcffda          c                   Const('FERMO DEPOSITO Aggiunto')
010633121213     d wcfin           c                   Const('INOLTRO Da:')
010634121213     d wctcpa          c                   Const('CONS.PARTICOLARI aggiunte:')
010635121213     d wctcpm          c                   Const('CONS.PARTIC. Da:')
010636121213     d wccasa          c                   Const('C/A aggiunto:')
010637121213     d wccasm          c                   Const('C/A Modif. Da:')
010638121213     d wciasa          c                   Const('IMPORTO ASS. Aggiunto:')
010639121213     d wciasm          c                   Const('IMPORTO ASS. Modif. Da:')
010640140131     d wcric           c                   Const('Tassazione Evento "RIC"')
010800060714
010900060714
011000060714      *------------------------------------------------------------------------*
011100060714      *  RIEPILOGO INDICATORI
011200060714      *------------------------------------------------------------------------*
011300061005
011400121127      *       -
011600060714
011700060714      *------------------------------------------------------------------------*
011800060714
011900121127      * Blocco la data penultima fatturazione mensile su tabella "RPF"
012000121127     c                   exsr      blk_data
012300121127      * elaboro variazioni post-fatturazione
012400060717     c                   exsr      Elabora
012500060718
012501121227     c                   exsr      routend
012700060718
012800060717      *------------------------------------------------------------------------*
012900121127      * Blocco la data penultima fatturazione tabella "RPF"
013000060717      *------------------------------------------------------------------------*
013100121127     c     blk_data      BegSr
017600121127      *
017601121127     c* Determinazione della data da memorizzare in tabella:
017602121206     c* - Sempre il primo giorno del mese
017603121127     c* - Se data del giorno dopo il 15 mantengo il mese/anno corrente
017604121127     c*   Se data del giorno il 15 o prima il mese è il precedente al corrente
017606121127     c                   z-add     woggi         §rpfdta
017607121127     c                   move      01            §rpfdta
017608121127     c                   if        %subdt(dataiso_cor:*days) <=15
017609121127     c                   eval      dataiso=%date(§rpfdta:*iso)
017610121127     c                   subdur    1:*m          dataiso
017611121127     c                   eval      §rpfdta=%dec(dataiso:*iso)
017612121127     c                   Endif
017613121127     c*
017700121127     c                   eval      kcode = 'RPF'
017800121127     c                   eval      kke1 =  'DATA'
017801121218
017802121218     a* aggiorno solo se non sono in simulazione
017803121218     c                   if        w_simul=*blanks
017804121218
017900060717     c     Ktbe          chain     tntbe000
018000060717      *
018100121127     c                   If        %found
018601121227     c                   movel     §rpfdta       tbeuni
018602121127     c                   update    tntbe000
018603121127     c                   Endif
018604121218
018605121218     c                   endif
018700060717      *
019700121127     c                   endsr
019900060717      *------------------------------------------------------------------------*
020000121127      * Elaboro il file TIVRB contenente le variazioni da recuperare
020100060717      *------------------------------------------------------------------------*
020200060717     c     Elabora       BegSr
020201150721
020202150721     c                   Clear                   dsQt1
020203150721     c                   Movel     'QT'          tblcod
020204150721     c                   Movel(p)  1             tblkey
020205150721     c     ktab          Chain     Tabel00f
020206150721     c                   If        %Found(Tabel00f) and TblFlg = *Blanks
020207150721     c                   Movel     TblUni        dsQt1
020208150721     c                   EndIf
020700060717
020800121206     c     *loval        setll     tivrb01l
020900121127    1c                   do        *hival
021000060717
021001121221     c                   if        w_simul='S'
021002121221     c                   read(n)   tivrb01l
021004121221     c                   else
021100121206     c                   read      tivrb01l
021101121221     c                   endif
021200060717
021300121206    2c                   if        %eof(tivrb01l)
021400060717     c                   leave
021500121127    2c                   endif
021501121206
021507121206     c* Scarto record con data fattura > della data ultima fatturaz.
021508121206     c* (le fatture settimanali dell' ultimo mese verranno recuperate il
021509121206     c* mese successivo)
021510121206    2c                   if        vrbdft>§rpfdta
021511121127     c                   iter
021512121127    2c                   endif
021513121213     c* record con causale variazione blanks: memorizzo tracciato record
021514121213     c* (questo record non lo flaggo subito come elaborato ma viene flaggato
021515121213     c* insieme al record "VA" dopo che è stata creata la bolla di recupero)
021516121211    2c                   if        vrbcvb=*blanks
021517121210     c                   eval      ds_vrbor=ds_vrb
021519121206     c                   iter
021520121206     c                   endif
021521121206     c* Verifico esclusioni per cliente e area
021522121206     c                   exsr      Record_ok
021523121206     c* Se recupero non previsto per area o cliente leggo altro record
021524121206    2c                   If        Rec_ok <>' '
021525130109     c                   eval      wflg=Rec_ok
021527121206     c                   exsr      updvrb
021528121206     c                   iter
021529121206    2c                   endif
021530121206     c* Impostazione campi per la tassazione
021531121206     c                   exsr      tassaz
021532121206     c                   if        dtas_taserr=*blanks and
021533121207     c                             (dtas_tasimv-f_tasimv)>1
021534130327     c* prima di procedere alla creazione della bolla verifico che ci siano
021535130327     c* effettivamente delle variazioni fra i dati del record originale e
021536130327     c* i dati del record contenente le variazioni. Se non ci fossero differenze
021537130327     c* la maggior tassazione dipenderebbe esclusivamente da una modifica alla
021538130327     c* tariffa ed in questo caso non bisogna recuperare nulla
021539130327     c                   exsr      ver_var
021540130327     c                   if        wnoRA=*blanks
021541130327     c                   eval      wflg='='
021542130327     c                   exsr      updvrb
021543130327     c                   iter
021544130327     c                   else
021545121206     c                   exsr      creabolla
021546121206     c                   endif
021547130327     c                   endif
021548121213     c* flaggo tivrb come elaborato a prescindere dalla creazione o meno della bolla
021549121213     c* di recupero
021550121217     c                   clear                   wflg
021551121217     c                   select
021552121217     c                   when      dtas_taserr<>*blanks
021553121217     c                   eval      wflg='E'
021554121217     c                   when      dtas_tasimv-f_tasimv<=1
021555121217     c                   eval      wflg='<'
021556121217     c                   endsl
021557121213     c                   exsr      updvrb
022200060717
022300121127    1c                   enddo
022301121127
022306121127
022900060717     c                   endsr
023000060717      *------------------------------------------------------------------------*
023100121127      * Verifico se spedizione da elaborare
023200060717      *------------------------------------------------------------------------*
023300060717     c     Record_ok     BegSr
023400060717
023500060717     c                   clear                   rec_ok
023503121127     c     ktas          chain     titas30c
023504121127     c                   if        not %found(titas30c)
023505121127     c                   eval      rec_ok = 'N'
023506121211     c                   leavesr
023507121127     c                   endif
023508121207     c                   movel     f_tasksc      filksc
023509130109     c* Verifico filiale cliente se da elaborare
023510130109     c* Ignoro questo controllo se simulazione su tutte le aree
023512121127     c     filksc        chain     azorg01l
023513130325     c                   if        w_area=*blanks and w_tuttea<>'S'
023514121127     c                   if        not %found(azorg01l) or
023515130325     c                             %lookup(%char(orgcar):sare)=0
023516130325     c****                         (%lookup(%char(orgcar):sare)=0 and
023517130325     c****                         %lookup('999':sare)=0)
023518121127     c                   eval      rec_ok = 'N'
023519121211     c                   leavesr
023520121211     c                   endif
023521130109     c                   endif
023522121127     c
023523121127     c* verifico cliente         se da elaborare
031200061006      * verifico l'anagrafica
031300061006     c                   Clear                   Tibs69ds
031401121207     c                   Z-add     f_tasksc      i69kcs
031402121221     c                   Z-add     f_tasksc      i69kac
031500061006     c                   Call      'TIBS69R'
031600061006     c                   Parm                    Tibs69ds
031700061006     c                   Parm                    ds_cnaco
031800061006     c                   Parm                    ds_cnind
031900061006     c                   Parm                    ds_cnclp
032000061006     c                   Parm                    ds_fncls
032100121127     c                   If        o69err = ' '  and %subst(clsflo:3:1)='N'
032300130109     c                   eval      rec_ok = 'C'
032400121211     c                   leavesr
032700061006     c                   endif
032701121221     c                   eval      s_acorag=acorag
032800060717
032900121211     c                   endsr
032901121206      *------------------------------------------------------------------------*
032902121206      * Tassazione bolla
032903121206      *------------------------------------------------------------------------*
032904121206     c     Tassaz        BegSr
032905150330
032906150330     C                   MOVEL     '3A'          tblcod
032907150330     C                   MOVEL(P)  f_tascbo      tblkey
032908150330     c                   clear                   ds3a
032909150330     C     KTAB          CHAIN     TABEL00f
032910150330     c                   if        %found(tabel00f)
032911150330     C                   MOVEL     TBLUNI        DS3a
032912150330     c                   endif
032913150330
032919121206     c                   clear                   dtas
032920121206     c                   clear                   dtasv
032921121207     c                   eval      dtas_TASKSC=f_tasksc
032922121207     c                   eval      dtas_TASNCL=f_tasncl
032923121207     c                   eval      dtas_TASCTR=f_tasctr
032924121207     c                   eval      dtas_TASFDN=f_tasfdn
032925121207     c                   eval      dtas_TASMCT=f_tasmct
032926121207     c                   eval      dtas_TASNZM=f_tasnzm
032927121207     c                   eval      dtas_TASCAM=f_tascam
032928121207     c                   eval      dtas_TASFAP=f_tasfap
032929121207     c                   eval      dtas_TASTSP=f_tastsp
032930121207     c                   eval      dtas_taslna=f_taslna
032931121207     c                   eval      dtas_TASDIV=f_tasdiv
032932121207     c                   eval      dtas_TASPVL=f_taspvl
032933121206     c                   eval      dtas_taspkf=VRBPKF
032934121206     c                   eval      dtas_taspkc=vRBPKC
032935121206     c                   eval      dtas_tasncp=VRBNCP
032936151026     C                   IF        §qtdst>0 and f_TASDFT<=§QTDST
032937150720     c* peso vdl: se spedizione fatturata prima della scadenza della tara
032938150720     c*           passo a tnsf20r il peso vdl a cui sottraggo la differenza di
032940150720     c*           tara fra quella vecchia e quella attuale moltiplicata per
032941150720     c*           il totale dei colli vdl
032942150720     c*           Serve per tassare con la stessa tara utilizzata al momento
032943150720     c*           della fatturazione
032944150720     C                   eval      dtas_taspkc=dtas_taspkc-((§qttco-§qttpc)*
032945150720     c                                                       dtas_tasncp)
032946150720     C                   ENDIF
032947121206     c                   eval      dtas_tasvlf=VRBVLF
032948121206     c                   eval      dtas_tascts=VRBCTS
032949121206     c                   eval      dtas_tasnzd=VRBNZD
032950121206     c                   eval      dtas_tascad=VRBCAD
032951121206     c                   eval      dtas_tasfin=VRBFIN
032952121206     c                   eval      dtas_tastc1=VRBTC1
032953121206     c                   eval      dtas_tastc2=VRBTC2
032954121206     c                   eval      dtas_taslnp=vrblnp
032955121206     c                   eval      dtas_TASTBL=vrbtbl
032956121206     c                   eval      dtas_tasvas=VRBVAS
032957121206     c                   eval      dtas_tasias=VRBIAS
032958121206     c                   eval      dtas_tasqft=VRBQFT
032959121206     c                   eval      dtas_tastic=VRBTIC
032960121206     c                   eval      dtas_tascas=VRBCAS
032961121206     c                   eval      dtas_tasvca=VRBVCA
032962121206     c                   eval      dtas_tascmb=VRBCMB
032963121206     c                   move      VRBSTA        dtas_tassta
032964140127     c                   eval      dtas_tasric=vrbric
032965121206     c                   clear                   dAr5Ban
032966121206     c                   Clear                   dAr5Bnb
032967121206     c                   Clear                   kpjbu
032968121207     c                   movel     f_tasaas      dtas_tasdsp
032969121207     c                   move      f_tasmgs      dtas_tasdsp
032970121206     c                   eval      dtas_tasdct=dtas_tasdsp
032971121206
032972121206      * Flag SI NO DDT
032973121207    2C                   If        f_tasll1 = 'C' or f_tasll1 = 'S'
032974121206     C                   movel     'S'           dtas_Tasddt
032975121206   x2C                   Else
032976121206     C                   Clear                   dtas_Tasddt
032977121206    2C                   Endif
032978121206     c                   clear                   dtas01
032979121207     c                   movel     f_tasfbr      §asfbr
032980121207     c                   movel     f_tascca      §ascca
032981121206      * valorizzo flag email al destinatario
032982121207     c                   movel     f_tasflo      dtasflo
032983121206     c                   move      §floemd       §asemd
032984150108     c* flag pin code
032985150108     c                   if        f_tasgma<>*blanks
032986150108     c                   clear                   ds7r
032987150108     c                   movel     '7R'          Tblcod
032988150108     c                   movel(p)  f_tasgma      Tblkey
032989150108     c     ktab          chain     tabel00f
032990150108    8c                   if        %found(tabel00f)
032991150108     c                   movel     tbluni        ds7r
032992150108    8c                   endif
032993150108     c                   if        §7rpincode='S'
032994150108     c                   movel     'S'           §aspin
032995150108     c                   endif
032996150108     c                   endif
032997121206
032998121206     c                   eval      dtas_tasflo = dtas01
032999160108     c* solo per prima bolla
033000160108     c                   if        §3atb2=*blanks or %subst(f_tastbl:1:1)
033001160108     c                             <>%subst(§3atb2:1:1)
033002160108     c
033003160108     c* flag DIRITTO DI CHIAMATA  da 10° byte di TASFLO di TITAS
033004160108     c                   move      §floado       dtas_tasprt
033005160108     c* flag per ADDEBITO PACKING LIST da fiar5 "GEN"
033006160108     c                   clear                   dar5gen
033007160108     c                   eval      kAr5Trd  = 'GEN'
033008160108     c     kFiar5        Chain     Fiar531c
033009160108If  3c                   If        %Found(Fiar531c)
033010160108     c                   Movel     Ar5Uni        dAr5gen
033011160108    3c                   EndIf
033012160108     c                   IF        §AR5als = 'S' or §AR5alx = 'S'
033013160108     c                   Eval      dtas_TasSpl = 'S'
033014160108     c                   endif
033015160108     c                   endif
033016121206
033017121206     c
033018121206      * Bancali
033019121207    2c                   If        %Subst(f_TasGva:1:1) = 'B'
033020121206
033021121206     c                   eval      kAr5Trd  = 'BAN'
033022121206     c     kFiar5        Chain     Fiar531c
033023121206If  3c                   If        %Found(Fiar531c)
033024121206     c                   Movel     Ar5Uni        dAr5Ban
033025121206    3c                   EndIf
033026121206      * Bancali
033027121206     c                   Eval      dtas_TasBan = §Ar5Nba + §Ar5Nb2
033028121206    2c                   EndIf
033029121206      **
033030121206      * Colli Originali
033031121207    2c                   If        %Subst(f_TasGva:1:1) = 'O'
033032121206     c                   eval      kAr5Trd  = 'BNB'
033033121206     c     kFiar5        Chain     Fiar531c
033034121206If  3c                   If        %Found(Fiar531c)
033035121206     c                   Movel     Ar5Uni        dAr5Bnb
033036121206    3c                   EndIf
033037121206      * Colli
033038121206     c                   Eval      dtas_TasNcl = §Ar5bcor
033039121206    2c                   EndIf
033040121206
033041121206      **** verifico se devo calcolare l'addebito di lasciato avviso
033042121206      **** verifico se c'è evento
033043140127     c***  Kevb4         chain     Fnevb04l
033044121206
033045140127    2c***                If        %found(Fnevb04l)
033046140127     c***                eval      dtas_tasric = 'S'
033047140127    2c***                endif
033048121206
033049121206      * Assicurazione
033050121207    2c                   IF        f_Tasfcm = 'F'
033051121206     c                   clear                   dtas_tasias
033052121206     c                   clear                   dtas_tasvas
033053121206    2c                   Endif
033054121206      * recupero i valori  già tassati
033055121206     c                   exsr      Ric_tai
033056150330
033057150330     c* se devo tassare solo una varia, da tipo bolla, la imposto
033058150330     c                   if        §3asva<>*blanks
033059150330     c                   eval      dtas_tassva=§3asva
033060150330     c                   endif
033061121206
033062121206     C                   CALL      'TNSF20R'
033063121206     C                   PARM                    KPJBA
033064121206     C                   PARM                    DTAS
033065121206     C                   PARM                    DTASV
033066121206
033067150330     c                   endsr
033068121206      *****************************************************************
033069121206      *  Recupero importi di tassazione dal file titai
033070121206      *****************************************************************
033071121206     c     Ric_tai       Begsr
033072121206      *
033073121206     c                   z-add     0             k
033074121206
033075121206     c     ktai05        setll     titai01l
033076121206
033077121206     c                   If        %equal(titai01l)
033078121206
033079121206     c                   do        *hival
033080121206
033081121206     c     ktai05        reade     titai01l
033082121206
033083121206     c                   If        %eof(titai01l)
033084121206     c                   leave
033085121206     c                   endif
033086121206      * imponibile
033087121206     c                   if        taisvt = '£'
033088121206     c                   eval      dtas_tasimv = taivat
033089121206     c                   endif
033090121206      * porto
033091121206     c                   if        taisvt = ' '
033092121206     c                   eval      dtas_taspor = taivat
033093121206     c                   endif
033094121206      * varie
033095121206     c                   if        taisvt <> ' ' and taisvt <> '£'
033096121206     c* se presente varia N 88.888.888 devo prima tassarla
033097121206    4c                   if        taisvt='N' and taivat=88888888
033098121206     c                   eval      savdtas=dtas
033099121206     c                   eval      savdtasv=dtasv
033100121206     c                   exsr      tassan
033101121206     c                   eval      dtas=savdtas
033102121206     c                   eval      dtasv=savdtasv
033103121206     c                   add       1             k
033104121206     c                   eval      sv(k) ='N'
033105121206     c                   eval      va(k) =antepo
033106121206   x4c                   else
033107121206     c                   add       1             k
033108121206     c                   eval      sv(k) = taisvt
033109121206     c                   eval      va(k) = taivat
033110121206    4c                   endif
033111121206     c                   endif
033112121206
033113121206     c                   enddo
033114121206
033115121206     c                   else
033116121206      * se non è pretassata pulisco la divisa
033117121206     c                   clear                   dtas_tasdiv
033118121206     c                   endif
033119121206     c                   endsr
033120121206      ****************************************************
033121121206      ** Tassazione varia N 88888888 (Anteporto)
033122121206      ****************************************************
033123121206     c     tassan        Begsr
033124121206     C                   CLEAR                   DTAS_tasimv
033125121206     C                   CLEAR                   DTAS_taspor
033126121206     C                   CLEAR                   DTASV
033127121206     c                   clear                   antepo
033128121206      * tolgo il flag dell'invio mail al destinatario nell'eventualità ci fosse
033129150108      * idem per Pin Code
033130160113      *                 e per diritto di chiamata e packing list
033131121206     c                   eval      dtas01 = dtas_tasflo
033132121206     c                   clear                   §asemd
033133150108     c                   clear                   §aspin
033134121206     c                   eval      dtas_tasflo = dtas01
033135160113     c                   clear                   dtas_tasprt
033136160113     c                   clear                   dtas_tasspl
033137121206      *
033138121206     C                   CALL      'TNSF20R'
033139121206     C                   PARM                    KPJBA
033140121206     C                   PARM                    DTAS
033141121206     C                   PARM                    DTASV
033142121206      *
033143121206      * Tassazione andata buon fine
033144121206     C     dtas_taserr   IFEQ      *BLANKS
033145121206     c                   eval      antepo=dtas_tasimv
033146121206     C                   ENDIF
033147121206     c                   endsr
033148121206      *------------------------------------------------------------------------*
033149121206      * Creazione bolla di recupero
033150121206      *------------------------------------------------------------------------*
033151121206     c     Creabolla     BegSr
033152121206
033153151016     c* Chaino Fiar5 record "FAT" e se c'è lo utilizzo per memorizzarlo anche
033154151016     c* sulla bolla di recupero anche nella memorizzazione di pkf e pkb
033155151026     c                   clear                   dar5fat
033156151026     c                   eval      kAr5Trd  = 'FAT'
033157151026     c     kFiar5        Chain     Fiar531c
033158151026If  3c                   If        %Found(Fiar531c)
033159151026     c                   eval      dar5fat=ar5uni
033160151026     c                   endif
033161121206
033162121206      * -- > T I T A S 0 0 0
033163121206      *---------------------
033164121206     c                   Clear                   Titas0
033165121206      * Anno
033166121211     c                   movel     §rpfdta       tasaas
033167121206      * Mese/giorno
033168121211     c                   movel     tasaas        tasdsp            8 0
033169121211     c                   move      §rpfdta       tasdsp
033170121207     c                   eval      dataiso=%date(tasdsp:*iso)
033171121206     c                   adddur    1:*m          dataiso
033172121207     c                   subdur    1:*d          dataiso
033173121207     c                   eval      tasdsp=%dec(dataiso)
033174121207
033175121207     c                   move      tasdsp        tasmgs
033176121207     c
033177121206      * Linea di partenza
033178121207     c                   Eval      taslnp=f_taslnp
033179121206
033180121221      * Codice bolla
033181121211     c                   Eval      TasCbo = 'BV'
033182121206      * Tipo bolla
033183121213     c                   Eval      TasTbl = s_§3atb1
033184121206      * codice cliente
033185121207     c                   Eval      tasksc = f_tasksc
033186121207      * cliente mittente
033187121207     c                   eval      Tasccm = f_tasksc
033188121207      * Linea di arrivo
033189121207     c                   Eval      taslna = f_TasLna
033190121206      * Numero Colli
033191121207     c                   Eval      TasNcl = F_TasNcl
033192151016     c* impostazione campi Peso se non presente il record FAT
033193151026     c                   if        §AR5PKTAS=0
033194121212      * Peso bollettato  = Peso Fatturato
033195150721      * il peso che memorizziamo è quello usato sulla bolla originale per
033196150721      * fatturare ovvero taspkf= V --> vdl - tara (taspkc)
033197150721      *                  taspkf<>V --> peso boll/variato (taspkf)
033198121213     c                   if        f_tasfpf<>'V'
033199121212     c                   Eval      TasPkb = F_TasPkf
033200121212     c                   Eval      tasPkf = f_TasPkf
033201121212     c                   else
033202150721     C                   IF        f_tASDFT<=§QTDST
033203150721     c     §QtTco        Mult      f_tasncp      wTara
033204150720     c                   else
033205121212     c     §QtTpc        Mult      f_tasncp      wTara
033206150721     c                   endif
033207121212     c     f_taspkc      Sub(h)    wTara         wNtara
033208121212     c                   Z-Add     wNtara        taspkb
033209121212     c                   Z-Add     wNtara        taspkf
033210121212     c                   endif
033211151016     c                   else
033212151016     c* Se presente il record "FAT"  prendo i pesi da titas della bolla "mamma" così
033213151016     c* come sono
033214151016     c                   Eval      TasPkb = F_TasPkb
033215151016     c                   Eval      tasPkf = f_TasPkf
033216151016     c                   Eval      tasPkc = f_TasPkc
033217151016     c                   Eval      tasNcp = f_TasNcp
033218151016     c                   endif
033219121207      * Volume bollettato + flag = Volume fatturato
033220121212     c                   Eval      TasVlb = F_Tasvlf
033221121212     c                   Eval      TasFvb = f_tasfvf
033222121212     c                   Eval      TasVlf = f_tasvlf
033223121212     c                   Eval      TasFvf = f_tasfvf
033224121207      * codice tariffa
033225121207     c                   Eval      tasctr=f_tasctr
033226121206      * Tipo servizio
033227121207     c                   Eval      TasTsp = F_tastsp
033228121207     c* determino se bolla originale è franco o assegnato
033229121207     c                   exsr      rep_dstb
033230121206      * Destinatario
033231121207      * -->  mittente o destinatario della bolla originale in base al porto
033232121207     c                   if        §tbtpo='F'
033233121207     c                   Eval      TasRsd = f_TasRsd
033234121207     c                   Eval      TasInd = f_TasInd
033235121207     c                   Eval      TasCad = f_TasCad
033236121207     c                   Eval      TasLod = f_TasLod
033237121207     c                   Eval      TasPrd = f_TasPrd
033238121207     c                   Eval      TasNzd = f_TasNzd
033239121207     c                   Eval      TasCpd = f_TasCpd
033240121207     c                   Eval      Tascts = f_tascts
033241121207     c                   Eval      Tasmct = f_tasmct
033242121207     c                   Eval      Tasfin = f_tasfin
033243121207     c                   Eval      Tasfap = f_tasfap
033244121207     c                   else
033245121211     c                   move      f_tasccm      wccm              4 0
033246121207     c* mittente bolla originale codificato
033247121207     c                   if        wccm<>8888
033248121207     c                   Clear                   Tibs69ds
033249121207     c                   Z-add     f_tasccm      i69kac
033250121207     c                   Z-add     f_tasccm      i69kin
033251121207     c                   Call      'TIBS69R'
033252121207     c                   Parm                    Tibs69ds
033253121207     c                   Parm                    ds_cnaco
033254121207     c                   Parm                    ds_cnind
033255121207     c                   Parm                    ds_cnclp
033256121207     c                   Parm                    ds_fncls
033257121207     c                   Eval      TasRsd = acorag
033258121207     c                   Eval      TasInd = indvia
033259121207     c                   Eval      TasCad = indCae
033260121207     c                   Eval      TasLod = indcit
033261121211     c                   Eval      TasPrd = indPrv
033262121207     c                   Eval      TasNzd = indsta
033263121207     c                   Eval      TasCpd = indiva
033264121207     c                   else
033265121207     c* cliente non codificato prendo dati da titaa
033266121207     c                   eval      ktrc = 'M'
033267121207     c     KTAStrc       CHAIN     TITAA30C
033268121207     c                   IF        %FOUND
033269121207     c                   movel     TAArsc        tasrsd
033270121207     c                   movel     TAAind        tasind
033271121207     c                   movel     TAAloc        taslod
033272121207     c                   movel     TAAnaz        tasnzd
033273121207     c                   movel     TAAprv        tasprd
033274121207     c                   movel     TAAcap        tascad
033275121207     c                   movel     TAAcpi        tascpd
033276121207     c                   ENDIF
033277121207     c                   endif
033278121207     c                   Eval      Tascts = f_tasmct
033279121207     c                   Eval      Tasmct = f_tascts
033280121207     c                   Eval      Tasfin = f_tasfap
033281121207     c                   Eval      Tasfap = f_tasfin
033282121207     c                   endif
033283121207      * Riferimento mittente numerico
033284121207     c                   z-add     f_TasRmn      TasRmn
033285121206      * Natura merce
033286121207     c                   Eval      tasNas = f_tasnas
033287121206      * P.o. bollettazione
033288121207     c                   Eval      TasFlb = Taslnp
033289121206      * Terminal Arrivo
033290121207     c                   Eval      TasTfa = F_TasTfa
033291121207     c                   clear                   fnlv55ds
033292121207     c                   eval      d55tpt='A'
033293121207     c                   eval      d55lin=taslna
033294121207     c                   eval      d55drf=*date
033295121207     c                   call      'FNLV55R'
033296121207     c                   parm                    fnlv55ds
033297121207     c                   if        d55err<>*blanks
033298121207     c                   leavesr
033299121207     c                   endif
033300121207     c                   eval      tastfa=d55tfa
033301121206      * Terminal partenza
033302121207     c                   clear                   fnlv55ds
033303121207     c                   eval      d55tpt='P'
033304121207     c                   eval      d55lin=taslnp
033305121207     c                   eval      d55drf=*date
033306121207     c                   call      'FNLV55R'
033307121207     c                   parm                    fnlv55ds
033308121207     c                   if        d55err<>*blanks
033309121207     c                   leavesr
033310121207     c                   endif
033311121207     c                   eval      tastfp=d55tfp
033312121206      * Data immissione
033313121207     c                   Eval      tasdim=woggi
033314121221      * Sigla operatore
033315121206     c                   Eval      TasSop = '*'
033316121206      * Codice autotrasportatore
033317121206     c                   Movel     TasLnp        TasPdr
033318121206     c                   Move      999           TasPdr
033319121206      * Data     ritiro
033320121206     c                   Eval      TasDrt = (tasaas * 10000) + TasMgs
033321121206      * Flag prestazione autotrasportatore
033322121206     c                   Eval      TasFpp = 'P'
033323121206      * Trattamento merce
033324121213     c                   Eval      TasCtm = s_§3actm
033325121206      * Data Borderò
033326121206     c                   Eval      TasDbr = (tasaas * 10000) + TasMgs
033327121206      * Flag merce portata a magazzino
033328121206     c                   Eval      TasFdn = 'S'
033329121206      * Flag ddt si/no
033330121206     c                   Eval      TasLl1 = 'Y'
033331121206      * Nazione e cap mittente
033332121206     c                   Clear                   Tibs69ds
033333121206     c                   Z-add     TasKsc        i69kin
033334121206     c                   Call      'TIBS69R'
033335121206     c                   Parm                    Tibs69ds
033336121206     c                   Parm                    ds_cnaco
033337121206     c                   Parm                    ds_cnind
033338121206     c                   Parm                    ds_cnclp
033339121206     c                   Parm                    ds_fncls
033340121206     c                   If        o69err = '1'
033341121207     c                   leavesr
033342121206     c                   EndIF
033343121206     c                   Eval      TasNzm = IndSta
033344121206     c                   Eval      TasCam = IndCae
033345121206      * divisa
033346121206     c                   Eval      TasDiv = 'EUR'
033347121207     c* Trasporto
033348121207     c                   Eval      Taspor = dtas_tasimv-f_tasimv
033349121207     c                   Eval      Tasimv = taspor
033350121206      * Falg bartolini/sdi
033351121206     c                   Eval      TasScl = 'B'
033352121206      * Numero spedizione (da Flnuf cod. 3)
033353121218     c                   if        w_simul=*blanks
033354121206     c                   Clear                   Trul34ds
033355121206     c                   Eval      i34Aas = TasAas
033356121206     c                   Eval      i34Lnp = TasLnp
033357121206     c                   Call      'TRUL34R'
033358121206     c                   Parm                    kpjba
033359121206     c                   Parm                    Trul34ds
033360121206     c                   If        f_o34err <> *Zeros
033361121207     c                   leavesr
033362121206     c                   EndIF
033363121218     c                   else
033364121218     c* In simulazione non stacco numero da flnuf ma utilizzo contatore interno partendo sempre
033365121218     c* dal numero 5000001
033366121218     c                   eval      w_contnum+=1
033367121218     c                   eval      o34nsp=w_contnum
033368121218     c                   endif
033369121206     c                   Eval      TasNsp = o34Nsp
033370121210
033371121210     c*----------------
033372121210     c* T I T A 4 0 0 0
033373121210     c*----------------
033374121210     c* creo record note per memorizzare il nuemro della bolla originale
033375121210     c                   clear                   tita40
033376121210     c                   eval      ta4aas=tasaas
033377121210     c                   eval      ta4lnp=taslnp
033378121210     c                   eval      ta4nrs=tasnrs
033379121210     c                   eval      ta4nsp=tasnsp
033380121210     c                   eval      ta4trc='8'
033381121210     c                   eval      ta4not='Bolla originale ' +
033382121210     c                             %editc(f_taslnp:'X')
033383121210     c                   if        f_tasnrs>0
033384121210     c                   eval      ta4not=%trim(ta4not) + ' ' +
033385121210     c                             %editc(f_tasnrs:'X') + '-' +
033386121213     c                             %trim(%editc(f_tasnsp:'4'))
033387121210     c                   else
033388121210     c                   eval      ta4not=%trim(ta4not) + ' ' +
033389121213     c                             %trim(%editc(f_tasnsp:'4'))
033390121210     c                   endif
033391121210     c                   eval      ta4not=%trim(ta4not) + ' ' +
033392121210     c                             %editc(f_tasaas:'X')
033393121213     c                   Write     Tita40
033394121211      * Riferimento mittente alfanumerico
033395121211     c                   clear                   s_§ta4arma
033396121211     c                   eval      ktrc='A'
033397121211     c     ktastrc       chain     tita430c
033398121211     c                   if        %found(tita430c)
033399121211     c                   movel     f_ta4not      dta4a
033400121211     c                   movel     §ta4arma      s_§ta4arma
033401121211     c                   endif
033402121218     c* solo se diverso da blanks
033403121218     c                   if        s_§ta4arma<>*blanks
033404121211     c                   clear                   tita40
033405121211     c                   eval      ta4aas=tasaas
033406121211     c                   eval      ta4lnp=taslnp
033407121211     c                   eval      ta4nrs=tasnrs
033408121211     c                   eval      ta4nsp=tasnsp
033409121211     c                   eval      ta4trc='A'
033410121211     c                   clear                   dta4a
033411121211     c                   eval      §TA4ARMA=s_§ta4arma
033412121211     c                   eval      ta4not=dta4a
033413121213     c                   Write     Tita40
033414121218     c                   endif
033415121210     c*----------------
033416121210     c* F I A R 5 0 0 0
033417121210     c*----------------
033418121210     c* creo su fiar5 legame bolla originale --> bolla di recupero
033419121210     c* e viceversa
033420121210     c* legame bolla di recupero --> bolla originale
033421121210     C                   clear                   fiar50
033422121210     C                   eval      ar5aas=tasaas
033423121210     C                   eval      ar5lnp=taslnp
033424121210     C                   eval      ar5nrs=tasnrs
033425121210     C                   eval      ar5nsp=tasnsp
033426121221     c*
033427121210     C                   eval      ar5trd='LEG'
033428121210     C                   eval      ar5uni=%editc(f_tasaas:'X') +
033429121210     c                             %editc(f_taslnp:'X') +
033430121210     c                             %editc(f_tasnrs:'X') +
033431121210     c                             %editc(f_tasnsp:'X')
033432121210     c                   eval      ar5dac=woggi
033433121210     c                   time                    ar5orc
033434121210     c                   write     fiar50
033435121210     c* legame bolla originale  --> bolla di recupero
033436121210     C                   clear                   fiar50
033437121210     C                   eval      ar5aas=f_tasaas
033438121210     C                   eval      ar5lnp=f_taslnp
033439121210     C                   eval      ar5nrs=f_tasnrs
033440121210     C                   eval      ar5nsp=f_tasnsp
033441121221     c*
033442121210     C                   eval      ar5trd='LEG'
033443121210     C                   eval      ar5uni=%editc(tasaas:'X') +
033444121210     c                             %editc(taslnp:'X') +
033445121210     c                             %editc(tasnrs:'X') +
033446121210     c                             %editc(tasnsp:'X')
033447121210     c                   eval      ar5dac=woggi
033448121210     c                   time                    ar5orc
033449121210     c                   write     fiar50
033450121210     c
033451151016     c* Creazione record "FAT" anche sulla bolla di recupero
033452151026     C                   if        §AR5PKTAS>0
033453151026     C                   clear                   fiar50
033454151026     C                   eval      ar5aas=tasaas
033455151026     C                   eval      ar5lnp=taslnp
033456151026     C                   eval      ar5nrs=tasnrs
033457151026     C                   eval      ar5nsp=tasnsp
033458151026     c*
033459151026     C                   eval      ar5trd='FAT'
033460151026     C                   EVAL      AR5UNI=DAR5FAT
033461151026     c                   eval      ar5dac=woggi
033462151026     c                   time                    ar5orc
033463151026     c                   write     fiar50
033464151026     C                   ENDIF
033465121210
033466121213     c                   Write     Titas0
033467121212
033468121212     c                   exsr      creaRA
033469121206
033470121207     c                   endsr
033471121206      *------------------------------------------------------------------------*
033472121206      * Creazione Richiesta Assistenza
033473121206      *------------------------------------------------------------------------*
033474121206     c     CreaRA        BegSr
033475140507     c                   clear                   fidna6ds
033476140507     c                   eval      ida06mad=' 30'
033477140507     c                   eval      ida06tor='S'
033478140507     c                   eval      ida06ogg=%editc(taslnp:'X')+
033479140507     c                             %editc(tasnrs:'X')+
033480121210     c                             %editc(tasnsp:'X')+%editc(tasaas:'X')
033481121210     c* verifica delle variazioni effettuate ed impostazione campo note da passare a pgm
033482130327     c* spostato il richiamo
033483130327     c*****              exsr      ver_var
033484140507     c                   eval      ida06no1=wnoRa
033485121218
033486121218     c* Crea richiesta assistenza solo se non in simulazione
033487121218     c                   if        w_simul=*blanks
033488121218
033489140507     c                   eval      ida06ute='EDPFAT    '
033490140507     c                   eval      ida06fil=dutpou
033491140507     c                   call      'FIDNA6R'
033493140507     c                   parm                    fidna6ds
033495121221     c                   endif
033496121218
033497121221     c* scrivo work_file
033498121221     c                   exsr      wrt_wf
033501121218
033502121218
033503121206     c                   endsr
033504121210      *------------------------------------------------------------------------*
033505121210      * Verifica variazioni effettuate
033506121210      *------------------------------------------------------------------------*
033507121210     c     ver_var       BegSr
033508121211     c                   clear                   wnoRa
033509130304     c* peso vdl o peso pkf in base a flag da tnsf20r
033510130304     c                   if        dtas_tasspc='S'
033511130304     c* peso VDL
033512130304     c                   if        or_vrbpkc<>vrbpkc
033513130304     c                   eval      wnoRa=wcpeso +
033514130304     c                             %trim(%editc(vrbpkc-or_vrbpkc:'4'))
033517130304     c                   eval      wnoRa=%trim(wnoRa) + ' Da VDL'
033519130304     c                   endif
033520130304     c                   else
033521130304     c* peso pkf
033522121210     c                   if        or_vrbpkf<>vrbpkf
033523121213     c                   eval      wnoRa=wcpeso +
033524121213     c                             %trim(%editc(vrbpkf-or_vrbpkf:'4'))
033525121210     c                   endif
033526130304     c                   endif
033527121210     c* volume
033528121213    1c                   if        or_vrbvlf<>vrbvlf
033529121213    2c                   if        wnoRA<>*blanks
033530121212     c                   exsr      ver_pos
033531121213    3c                   if        %subst(wnora:279:2)='++'
033532121212     c                   leavesr
033533121213    3c                   endif
033534121213     c                   eval      %subst(wnoRa:Pos)=wcvolu
033535121213   x2c                   else
033536121213     c                   eval      wnoRa=wcvolu
033537121213    2c                   endif
033538121211     c                   eval      wnoRa=%trim(wnoRa) + ' ' +
033539121213     c                             %trim(%editc(vrbvlf-or_vrbvlf:'4'))
033540130301     c* se da Vdl lo scrivo
033541130301     c                   if        vrbfvf='Z' or vrbfvf='T'
033542130301     c                   eval      wnoRa=%trim(wnoRa) + ' Da VDL'
033543130301     c                   endif
033544121213    1c                   endif
033545121211     c* Nazione destinatario
033546121211     c                   if        or_vrbnzd<>vrbnzd
033547121211     c                   if        wnoRA<>*blanks
033548121212     c                   exsr      ver_pos
033549121212     c                   if        %subst(wnora:279:2)='++'
033550121212     c                   leavesr
033551121212     c                   endif
033552121213     c                   eval      %subst(wnoRa:Pos)=wcnzd
033553121211     c                   else
033554121213     c                   eval      wnoRa=wcnzd
033555121211     c                   endif
033556121211     c                   eval      wnoRa=%trim(wnora) + ' ' +
033557121213     c                             or_vrbnzd + ' A: ' + vrbnzd
033558121211     c                   endif
033559121211     c* Cap destinatario
033560121211     c                   if        or_vrbcad<>vrbcad
033561121211     c                   if        wnoRA<>*blanks
033562121212     c                   exsr      ver_pos
033563121212     c                   if        %subst(wnora:279:2)='++'
033564121212     c                   leavesr
033565121212     c                   endif
033566121212     c                   eval      %subst(wnoRa:Pos)=
033567121213     c                             wccad
033568121211     c                   else
033569121213     c                   eval      wnoRa=wccad
033570121211     c                   endif
033571121211     c                   eval      wnoRa=%trim(wnora) + ' ' +
033572121213     c                             %trim(or_vrbcad) + ' A: ' +
033573121212     c                             vrbcad
033574121211     c                   endif
033575121210     c* Cod.tassazione
033576121210     c                   if        or_vrbcts<>vrbcts
033577121211     c                   if        wnoRA<>*blanks
033578121212     c                   exsr      ver_pos
033579121212     c                   if        %subst(wnora:279:2)='++'
033580121212     c                   leavesr
033581121212     c                   endif
033582121212     c                   eval      %subst(wnoRa:Pos)=
033583121213     c                             wccts
033584121211     c                   else
033585121213     c                   eval      wnoRa=wccts
033586121211     c                   endif
033587121211     c                   eval      wnoRa=%trim(wnora) + ' ' +
033588121213     c                             or_vrbcts + ' A: ' + vrbcts
033589121210     c                   endif
033590121211     c* Fermo deposito
033591121211     c                   if        or_vrbffd<>vrbffd
033592121211     c                   if        or_vrbffd<>*blanks and vrbffd=*blanks
033593121211     c                   if        wnoRA<>*blanks
033594121212     c                   exsr      ver_pos
033595121212     c                   if        %subst(wnora:279:2)='++'
033596121212     c                   leavesr
033597121212     c                   endif
033598121212     c                   eval      %subst(wnoRa:Pos)=
033599121213     c                             wcffdt
033600121211     c                   else
033601121213     c                   eval      wnoRa=wcffdt
033602121211     c                   endif
033603121211     C                   else
033604121211     c                   if        wnoRA<>*blanks
033605121212     c                   exsr      ver_pos
033606121212     c                   if        %subst(wnora:279:2)='++'
033607121212     c                   leavesr
033608121212     c                   endif
033609121212     c                   eval      %subst(wnoRa:Pos)=
033610121213     c                             wcffda
033611121211     c                   else
033612121213     c                   eval      wnoRa=wcffda
033613121211     c                   endif
033614121211     c                   endif
033615121211     c                   endif
033616121210     c* Flag inoltro
033617121210     c                   if        or_vrbfin<>vrbfin
033618121211     c                   if        wnoRA<>*blanks
033619121212     c                   exsr      ver_pos
033620121212     c                   if        %subst(wnora:279:2)='++'
033621121212     c                   leavesr
033622121212     c                   endif
033623121212     c                   eval      %subst(wnoRa:Pos)=
033624121213     c                             wcfin
033625121211     c                   else
033626121213     c                   eval      wnoRa=wcfin
033627121211     c                   endif
033628121213     c                   eval      wnoRa=%trim(wnora) + ' "' +
033629121213     c                             or_vrbfin + '" a: ' + '"' + vrbfin
033630121213     c                             + '"'
033631121210     c                   endif
033632121210     c* Consegne particolari
033633121213    1c                   if        (or_vrbtc1=vrbtc1   and
033634121210     c                             or_vrbtc2=vrbtc2) or
033635121210     c                             (or_vrbtc1=vrbtc2   and
033636121210     c                             or_vrbtc2=vrbtc1)
033637121213   x1c                   else
033638121213    2c                   select
033639121213     c* . . . . . . Aggiunte
033640121213     c                   when      or_vrbtc1=*blanks and or_vrbtc2=*blanks
033641121213    3c                   if        wnoRA<>*blanks
033642121213     c                   exsr      ver_pos
033643121213    4c                   if        %subst(wnora:279:2)='++'
033644121213     c                   leavesr
033645121213    4c                   endif
033646121213     c                   eval      %subst(wnoRa:Pos)=
033647121213     c                             wctcpa
033648121213   x3c                   else
033649121213     c                   eval      wnoRa=wctcpa
033650121213    3c                   endif
033651121213     c                   if        vrbtc1<>*blanks
033652121213     c                   eval      wnoRa=%trim(wnora) + ' "' +
033653121213     c                             vrbtc1 + '"'
033654121213     c                   endif
033655121213     c                   if        vrbtc2<>*blanks
033656121213     c                   eval      wnoRa=%trim(wnora) + ' "' +
033657121213     c                             vrbtc2 + '"'
033658121213     c                   endif
033659121213     c                   other
033660121213     c* . . . . . . Modificate
033661121213    3c                   if        wnoRA<>*blanks
033662121212     c                   exsr      ver_pos
033663121213    4c                   if        %subst(wnora:279:2)='++'
033664121212     c                   leavesr
033665121213    4c                   endif
033666121212     c                   eval      %subst(wnoRa:Pos)=
033667121213     c                             wctcpm
033668121213   x3c                   else
033669121213     c                   eval      wnoRa=wctcpm
033670121213    3c                   endif
033671121213     c                   if        or_vrbtc1<>*blanks
033672121213     c                   eval      wnoRa=%trim(wnora) + ' "' +
033673121213     c                             or_vrbtc1 + '"'
033674121213     c                   endif
033675121213     c                   if        or_vrbtc2<>*blanks
033676121213     c                   eval      wnoRa=%trim(wnora) + ' "' +
033677121213     c                             or_vrbtc2 + '"'
033678121213     c                   endif
033679121213     c                   eval      wnoRa=%trim(wnora) +
033680121213     c                             ' a:'
033681121213     c                   if        vrbtc1<>*blanks
033682121213     c                   eval      wnoRa=%trim(wnora) + ' "' +
033683121213     c                             vrbtc1 + '"'
033684121213     c                   endif
033685121213     c                   if        vrbtc2<>*blanks
033686121213     c                   eval      wnoRa=%trim(wnora) + ' "' +
033687121213     c                             vrbtc2 + '"'
033688121213     c                   endif
033689121213    2c                   endsl
033690121213    1c                   endif
033691121210     c* Contrassegno
033692121210     c                   if        Or_vrbcas<>vrbcas or
033693121210     c                             Or_vrbtic<>vrbtic or
033694121210     c                             Or_vrbvca<>vrbvca or
033695121211     c                             Or_vrbcmb<>vrbcmb or
033696121211     c                             (Or_vrbsta<>vrbsta and vrbsta<>9)
033697121211     c                   select
033698121210     c* . . . . Contrassegno annullato
033699121211     c***                when      Or_vrbsta<>9 and vrbsta=9
033700121211     c***                eval      wnoRa='Contrassegno ANNULLATO'
033701121211     c* . . . . Contrassegno aggiunto
033702121211     c                   when      or_vrbcas= 0 and vrbcas>0
033703121211     c                   if        wnora<>*blanks
033704121212     c                   exsr      ver_pos
033705121212     c                   if        %subst(wnora:279:2)='++'
033706121212     c                   leavesr
033707121212     c                   endif
033708121212     c                   eval      %subst(wnoRa:Pos)=
033709121213     c                             wccasa
033710121211     c                   else
033711121213     c                   eval      wnoRa=wccasa
033712121211     c                   endif
033713121211     c                   eval      wnoRA=%trim(wnora) + ' '  +
033714121211     c                             vrbvca + ' ' +
033715121212     c                             %trim(%editc(vrbcas:'4'))
033716121211     c                   if        vrbtic = ' M'
033717121212     c                   eval      wnoRA=%trim(wnora) + ' Mittente'
033718121211     c                   else
033719121212     c                   eval      wnoRA=%trim(wnora) + ' Vettore'
033720121211     c                   endif
033721121211     c* . . . . Contrassegno modificato
033722121211     c                   other
033723121211     c                   if        wnora<>*blanks
033724121212     c                   exsr      ver_pos
033725121212     c                   if        %subst(wnora:279:2)='++'
033726121212     c                   leavesr
033727121212     c                   endif
033728121212     c                   eval      %subst(wnoRa:Pos)=
033729121213     c                             wccasm
033730121211     c                   else
033731121213     c                   eval      wnoRa=wccasm
033732121211     c                   endif
033733121211     c                   eval      wnoRA=%trim(wnora) + ' '  +
033734121212     c                             Or_vrbvca + ' ' +
033735121212     c                             %trim(%editc(or_vrbcas:'4'))
033736121211     c                   if        Or_vrbtic = ' M'
033737121212     c                   eval      wnoRA=%trim(wnoRa) + ' Mittente'
033738121211     c                   else
033739121212     c                   eval      wnoRA=%trim(wnoRa) + ' Vettore'
033740121211     c                   endif
033741121213     c                   eval      wnoRA=%trim(wnoRa) + ' A: ' +
033742121212     c                             vrbvca + ' ' +
033743121212     c                             %trim(%editc(vrbcas:'4'))
033744121211     c                   if        vrbtic = ' M'
033745121212     c                   eval      wnoRA=%trim(wnoRa) + ' Mittente'
033746121211     c                   else
033747121212     c                   eval      wnoRA=%trim(wnoRa) + ' Vettore'
033748121211     c                   endif
033749121211     c                   endsl
033750121211     c* . . . . . Scrivo se poi annullato
033751121211     c                   if        vrbsta=9
033752121211     c                   eval      wnoRA=%trim(wnora) + ' poi annullato'
033753121211     c                   endif
033754121211     c                   endif
033755121210     c* Importo da assicurare
033756121211     c                   if        Or_vrbvas<>vrbvas or
033757121211     c                             Or_vrbias<>vrbias
033758121211     c                   select
033759121211     c* . . . . . importo da assicurare aggiunto
033760121211     c                   when      or_vrbias=0
033761121212     c                   if        wnora<>*blanks
033762121212     c                   exsr      ver_pos
033763121212     c                   if        %subst(wnora:279:2)='++'
033764121212     c                   leavesr
033765121212     c                   endif
033766121212     c                   eval      %subst(wnoRa:Pos)=
033767121213     c                             wciasa
033768121211     c                   else
033769121213     c                   eval      wnoRa=wciasa
033770121211     c                   endif
033771121211     c                   eval      wnoRA=%trim(wnora) + ' '  +
033772121211     c                             vrbvas + ' ' + %editc(vrbias:'4')
033773121211     c
033774121211     c* . . . . . . importo da assicurare modificato
033775121211     c                   when      vrbias>0
033776121211     c                   if        wnora<>*blanks
033777121212     c                   exsr      ver_pos
033778121212     c                   if        %subst(wnora:279:2)='++'
033779121212     c                   leavesr
033780121212     c                   endif
033781121212     c                   eval      %subst(wnoRa:Pos)=
033782121213     c                             wciasm
033783121211     c                   else
033784121213     c                   eval      wnoRa=wciasm
033785121211     c                   endif
033786121211     c                   eval      wnoRA=%trim(wnora) + ' '  +
033787121212     c                             or_vrbvas + ' ' +
033788121212     c                             %trim(%editc(or_vrbias:'4')) +
033789121213     c                             ' A: ' + vrbvas + ' ' +
033790121211     c                             %editc(vrbias:'4')
033791121211     c                   endsl
033792121211     c                   endif
033793140124     c* Flag evento RIC
033794140124     c                   if        or_vrbric= ' ' and vrbric='S'
033795140124     c                   if        wnoRA<>*blanks
033796140124     c                   exsr      ver_pos
033797140124     c                   if        %subst(wnora:279:2)='++'
033798140124     c                   leavesr
033799140124     c                   endif
033800140124     c                   eval      %subst(wnoRa:Pos)=
033801140124     c                             wcric
033802140124     c                   else
033803140124     c                   eval      wnoRa=wcric
033804140124     c                   endif
033808140124     c                   endif
033809121210     c                   endsr
033810121206      *------------------------------------------------------------------------*
033811121206      * Aggiorna TIVRB come elaborato
033812121213     c*------------------------------------------------------------------------*
033813121206     c     Updvrb        BegSr
033814121218
033815121218     c* SOLO SE NON IN SIMULAZIONE
033816121218     c                   if        w_simul=*blanks
033817121218
033818121213     c* flaggo sia il record originale che il record "VA"
033819121213     c     kvrb          readpe    tivrb01l
033820121213     c                   z-add     woggi         vrbdela
033821121217     c                   eval      vrbflg=wflg
033822121213     c                   update    tivrb000
033823121213     c     kvrb          reade     tivrb01l
033824121206     c                   z-add     woggi         vrbdela
033825121217     c                   eval      vrbflg=wflg
033826121206     c                   update    tivrb000
033827121218
033828121218     c                   endif
033829121218
033830121206     c                   endsr
033831121212      *------------------------------------------------------------------------*
033832121212      * Verifica posizione per sottostringa su nota RA
033833121212      *------------------------------------------------------------------------*
033834121212     c     ver_pos       BegSr
033835121212     c                   clear                   pos
033836121212     c                   eval      pos=%checkr(' ':wnora)
033837121212     c                   if        pos>=279
033838121212     c                   eval      %subst(wnora:279:2)='++'
033839121212     c                   else
033840121212     c                   eval      risult=%div(pos:35)
033841121212     c                   eval      rem=%rem(pos:35)
033842121212     c                   if        rem>0
033843121212     c                   eval      risult+=1
033844121212     c                   endif
033845121212     c                   eval      pos=(risult*35)+1
033846121212     c                   if        pos>=279
033847121212     c                   eval      %subst(wnora:279:2)='++'
033848121212     c                   endif
033849121212     c                   endif
033850121212     c                   endsr
033851121207      *------------------------------------------------------------------------*
033852121218      * Recupera dstb
033853121207      *------------------------------------------------------------------------*
033854121207     c     rep_dstb      BegSr
033855121207     c                   Clear                   dstb
033856121207     c                   Eval      TblCod = 'TB'
033857121207     c                   Movel(p)  f_tastbl      TblKey
033858121207     c     kTab          Chain     Tabel00f
033859121207     c                   if        %found(tabel00f)
033860121207     c                   eval      dstb=tbluni
033861121207     c                   endif
033862121207     c                   endsr
033863121221      *------------------------------------------------------------------------*
033864121221      * Scrittura Work-file
033865121221      *------------------------------------------------------------------------*
033866121221     c     wrt_wf        BegSr
033867121221     c                   clear                   wfrpf000
033868121221     c* filiale del cliente
033869121221     c                   movel     tasksc        rpffil
033870121221     c                   eval      RPFFID=orgdes
033871121221     c* distretto
033872121221     c                   eval      RPFDIV=orgfl3
033873121221     c                   clear                   ds17
033874121221     c                   Eval      TblCod = '17'
033875121221     c                   Movel(p)  rpfdiv        TblKey
033876121221     c     kTab          Chain     Tabel00f
033877121221     c                   if        %found(tabel00f)
033878121221     c                   movel     tbluni        ds17
033879121221     c                   endif
033880121221     c                   eval      RPFDID=§17des
033881121221     c* area
033882121221     c                   eval      RPFARE=orgcar
033883121221     c                   clear                   ds05
033884121221     c                   Eval      TblCod = '05'
033885121221     c                   Movel(p)  rpfare        TblKey
033886121221     c     kTab          Chain     Tabel00f
033887121221     c                   if        %found(tabel00f)
033888121221     c                   movel     tbluni        ds05
033889121221     c                   endif
033890121221     c                   eval      RPFARD=§05des
033891121221
033892121221     c                   eval      RPFDAT=woggi
033893121221     c                   eval      RPFAASR=tasaas
033894121221     c                   EVAL      RPFLNPR=TASLNP
033895121221     c                   EVAL      RPFNRSR=tasnrs
033896121221     c                   eval      RPFNSPR=tasnsp
033897121221     c                   eval      RPFIMV =tasimv
033898121221     c                   eval      RPFKSC =tasksc
033899121221     c                   eval      RPFRAG=s_acorag
033900140507     c                   eval      RPFMTR=ida06no1
033901121221     c                   eval      RPFAASO=f_tasaas
033902121221     c                   eval      RPFLNPO=f_taslnp
033903121221     c                   eval      RPFNRSO=f_tasnrs
033904121221     c                   eval      RPFNSPO=f_tasnsp
033905121221     c                   eval      RPFMGSO=f_tasmgs
033906121221
033907121221     c                   if        w_simul='S'
033908121221     c                   write     wfrpfs
033909121221     c                   else
033910121221     c                   write     wfrpf000
033911121221     c                   endif
033912121221
033913121221     c                   endsr
055100060717      *------------------------------------------------------------------------*
055200060714      * Routine iniziale
055300060714      *------------------------------------------------------------------------*
055400060714     c     *Inzsr        BegSr
055500060714
055600060714     c     *Entry        Plist
055700060714     c                   Parm                    kpjba
055701121218     c                   movel     kpjbu         w_simul
055702130109     c                   eval      w_area=%subst(kpjbu:2:1)
055800060717
055801121221     c* Apertura del file di work
055802121218     c                   if        w_simul='S'
055804121221     c                   open      wfrpf00s
055805121221     c                   else
055806121221     c                   open      wfrpf00f
055807121221     c                   endif
056000060714
056100060714     c     *dtaara       define    §azute        azuteds
056200060714     c     *dtaara       define    §datiute      ddatiute
056300060714     c                   in(E)     *dtaara
056400060714     c                   If        %error  or RSUT = *blanks
056500060714     c                   Clear                   Tibs34ds
056600060714     c                   Call      'TIBS34R'
056700060714     c                   Parm                    Tibs34ds
056800060714     c                   In        *dtaara
056900060714     c                   EndIf
057000060714
057001121127      * reperisco la data corrente
057002121127     c                   time                    dataiso_cor
057003121127     c                   move      dataiso_cor   woggi
057004121127     c                   eval      gg=%subdt(dataiso_cor:*days)
057700060717      * chiavi
057800060717
057900060717      * Tntbe01l
058000060717     c     Ktbe          Klist
058100060717     c                   Kfld                    Kcode
058200060717     c                   Kfld                    Kke1
058201121127      * Titas30c
058202121127     c     kTas          klist
058203121127     c                   kfld                    vrbAas
058204121127     c                   kfld                    vrbLnp
058205121127     c                   kfld                    vrbNrs
058206121127     c                   kfld                    vrbNsp
058207121127     c                   kfld                    vrbtbl
058208121207     C     ktastrc       KLIST
058209121207     C                   KFLD                    vrbAAS
058210121207     C                   KFLD                    vrbLNP
058211121207     C                   KFLD                    vrbNRS
058212121207     C                   KFLD                    vrbNSP
058213121207     C                   KFLD                    Ktrc
059000060721      * Tabel00f
059100060721     c     Ktab          Klist
059200060721     c                   kfld                    TblKut
059300060721     c                   kfld                    TblCod
059400060721     c                   kfld                    TblKey
059401121211     c     kFiar5        Klist
059402121211     c                   Kfld                    vrbAas
059403121211     c                   Kfld                    vrbLnp
059404121211     c                   Kfld                    vrbNrs
059405121211     c                   Kfld                    vrbNsp
059406121211     c                   Kfld                    kAr5Trd
059407121211      * Chiave file eventi FNEVB
059408140127     c*    Kevb4         Klist
059409140127     c*                  Kfld                    vrbaas
059410140127     c*                  Kfld                    vrblnp
059411140127     c*                  Kfld                    vrbnrs
059412140127     c*                  Kfld                    vrbnsp
059413140127     c*                  Kfld                    Wcev
059414121211     c     ktai05        klist
059415121211     c                   kfld                    vrbaas
059416121211     c                   kfld                    vrblnp
059417121211     c                   kfld                    vrbnrs
059418121211     c                   kfld                    vrbnsp
059419121211     c                   kfld                    vrbtbl
059420121213     c     kvrb          klist
059421121213     c                   kfld                    vrbaas
059422121213     c                   kfld                    vrblnp
059423121213     c                   kfld                    vrbnrs
059424121213     c                   kfld                    vrbnsp
059425121213     c                   kfld                    vrbtbl
059500060718
059600060721     c                   Eval      TblKut = 1
059601121213
059602121213     c* Chain tabella 3a con codice bolla utilizzato per creare le bolle di recupero
059603121213     c* variazioni post-fatturazione
059604121213      * Tipo bolla
059605121213     c                   Clear                   ds3a_sav
059606121213     c                   Eval      TblCod = '3A'
059607121213     c                   Movel(p)  'BV'          TblKey
059608121213     c     kTab          Chain     Tabel00f
059609121213     c                   If        Not %Found(Tabel00f) or TblFlg <> *Blanks
059610121213     c                   eval      *inlr=*on
059611121213     c                   return
059612121213     c                   endif
059613121213     c                   Eval      ds3a_sav = TblUni
059614121206
059615121206     c                   exsr      rec_RPFarea
059700060717
059800060714     c                   EndSr
059801121227      *------------------------------------------------------------------------*
059802121227      * Routine finale
059803121227      *------------------------------------------------------------------------*
059804121227     c     routend       BegSr
059805121227
059806121227     C                   MOVEL     'C'           T02TLA
059807121227     C                   CALL      'TIBS02R'
059808121227     C                   PARM                    KPJBA
059809121227     C                   PARM                    tibs02ds
059810121227
059811121227     c                   Eval      i69tla = 'C'
059812121227     c                   Call      'TIBS69R'
059813121227     c                   Parm                    tibs69ds
059814121227
059815121227     C                   MOVEL     'C'           D55TLA
059816121227     C                   CALL      'FNLV55R'
059817121227     C                   PARM                    FNLV55DS
059818121227
059819121227     c                   Eval      *InLr = *On
059820121227
059821121227     c                   EndSr
059900121206      ****************************************************
060000121206      ** Recupero aree clienti elaborabili
060100121206      ****************************************************
060200121206     c     rec_RPFarea   Begsr
060300121206     c                   Clear                   Tibs02ds
060400121211     c                   Clear                   camposare
060401130325     c                   Clear                   w_tuttea
060500121206     c                   Eval      T02Mod = 'C'
060600121206     c                   Eval      T02Sif = knsif
060700121206     c                   Eval      T02Cod = 'RPF'
060800121206     c                   Movel(p)  'AREA'        T02Ke1
060900121206     c                   Call      'TIBS02R'
061000121206     c                   Parm                    Kpjba
061100121206     c                   Parm                    Tibs02ds
061200121206     c                   If        T02Err = *Blanks
061300121211     c                   eval      camposare=t02uni
061400121206     c                   EndIf
061401130325     c                   if        %lookup('999':sare)>0
061402130325     c                   eval      w_tuttea='S'
061403130325     c                   endif
061500121206     c                   endsr
