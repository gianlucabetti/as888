000100121212     H DEBUG DECEDIT('0,') DATEDIT(*YMD.) option(*nodebugio)
000200060718     H*------------------------------------------------------------------------*
000300121127     Ftntbe01l  uf   e           k Disk
000400121206     Ftivrb01l  uf   e           k Disk
000500121207     Ftitas30c  if   e           k Disk    prefix(f_)
000600121207     Ftitaa30c  if   e           k Disk
000700121211     Ftita430c  if   e           k Disk    prefix(f_)
000800060721     FTabel00f  if   e           k Disk
000900060714     Fazorg01l  if   e           k Disk
001000121206     Ffiar531c  if   e           k disk
001100121206     fTitaI01L  if   e           k disk
001200140127     F***fnevb04l  if   e           k disk
001300121207     fTitas00f  o    e             Disk    rename(Titas000:Titas0)
001400121127     fTita400f  o    e             Disk    rename(Tita4000:Tita40)
001500121210     ffiar500f  o    e             Disk    rename(fiar5000:fiar50)
001600121221     fwfrpf00F  o    e             Disk    usropn
001700121221     fwfrpf00S  o    e             Disk    rename(wfrpf000:wfrpfs)
001800121221     f                                     usropn
001900060714      *------------------------------------------------------------------------*
002000060714      *   S C H I E R E
002100060714      *------------------------------------------------------------------------*
002200121211     d camposare       ds           255
002300121211     d  sare                   1    255    DIM(85)                              Codici Area
002400121102     d
002500060714      *------------------------------------------------------------------------*
002600060714      *   V A R I A B I L I
002700060714      *------------------------------------------------------------------------*
002800060714
002900121218     D w_simul         s              1                                         flag simulazione
003000130110     D w_area          s              1
003100130325     D w_tuttea        s              1
003200130110     D w_contnum       s              7  0 inz(5000000)
003300121218     D k               s              5  0                                      indice
003400121212     D pos             s              3  0                                      indice
003500121212     D rec_ok          s              1
003600121127     d Kcode           S                   like(tbecod)
003700121127     d Kke1            S                   like(tbeke1)
003800121207     d ktrc            s                   like(TaaTrc)
003900121211     d kAr5Trd         s                   Like(Ar5Trd)
004000140127     D*Wcev            s                   like(evbcev) inz('RIC')
004100121211     d savdtas         s                   Like(dtas)
004200121211     d savdtasv        s                   Like(dtasv)
004300121211     d antepo          s                   Like(tasimv)
004400121211     d s_§ta4arma      s                   Like(§ta4arma)
004500160122     d s_§ta4anas      s                   Like(§ta4anas)
004600121212     d wTara           s                   Like(§QtTpc)
004700121212     d wNtara          s                   Like(TasPkf)
004800121212     d rem             s              5  0
004900121212     d risult          s              5  0
005000061006
005100121127     d dataiso_cor     s               d   datfmt(*iso)
005200121127     d gg              s              2  0
005300121127     d woggi           s              8  0
005400121127     d dataiso         s               d   datfmt(*iso)
005500061006     d dataeur         s               d   datfmt(*eur)
005600121206     d filksc          s              3  0
005700140507     d wnoRA           s                   like(ida06no1)
005800121217     d wflg            s                   like(vrbflg)
005900121221     d s_acorag        s                   like(acorag)
006000060717
006100060714      *------------------------------------------------------------------------*
006200060714      *   D S   I N T E R N E / E S T E R N E
006300060714      *------------------------------------------------------------------------*
006400060717
006500060714
006600060714     d Azuteds       e ds                  extname(Azute00f)
006700060714     d dDatiute      e ds
006800060714     d ds_cnaco      e ds                  extname(Cnaco00f)
006900060714     d ds_cnind      e ds                  extname(Cnind00f)
007000060714     d ds_cnclp      e ds                  extname(Cnclp00f)
007100060714     d ds_fncls      e ds                  extname(Fncls00f)
007200121210     d ds_vrb        e ds                  extname(tivrb00f)
007300121210     d ds_vrbOr      e ds                  extname(tivrb00f)  prefix(Or_)
007400060714     d Kpjba         e ds
007500060714     d Tibs34ds      e ds
007600060714     d Tibs69ds      e ds
007700121207     d fnlv55ds      e ds
007800060714     d Trul34ds      e ds
007900060714     d  f_i34tla     e                     extfld(i34tla)
008000060714     d  f_o34err     e                     extfld(o34err)
008100060714     d  f_o34msg     e                     extfld(o34msg)
008200140507     d fidna6ds      e ds
008300121211     d tibs02ds      e ds
008400121211
008500121211     d drpfd         e ds
008600121213     d ds3a_sav      e ds                  extname(ds3a) prefix(s_)
008700150330     d ds3a          e ds                  extname(ds3a)
008800121221     d ds17          e ds
008900121221     d ds05          e ds
009000150108     d ds7R          e ds
009100121128
009200121128      ** DS Calcolo tassazione
009300121206     d dtas          e ds                  prefix(dtas_)
009400121206     d dtasv         e ds                  prefix(dtasv_)
009500121128     d  sv                     1     20    dim(20)
009600121128     d  va                    21    140P 3 dim(20)
009700121206     d dar5ban       e ds
009800121206     d dar5bnb       e ds
009900151026     d dar5fat       e ds
010000160108     d dar5gen       e ds
010100121207     d dstb          e ds
010200121211     d dtasflo       e ds
010300121211     d dtas01        e ds
010400121211     d dta4a         e ds
010500121212     d dsqt1         e ds
010600121213      * Costanti per note R.A.
010700121213     d wcvolu          c                   Const('VOLUME +')
010800121213     d wcpeso          c                   Const('PESO   +')
010900121213     d wcnzd           c                   Const('NAZ.DESTIN. Da:')
011000121213     d wccad           c                   Const('CAP DESTIN. Da:')
011100121213     d wccts           c                   Const('COD.TASSAZ. Da:')
011200121213     d wcffdt          c                   Const('FERMO DEPOSITO Tolto')
011300121213     d wcffda          c                   Const('FERMO DEPOSITO Aggiunto')
011400121213     d wcfin           c                   Const('INOLTRO Da:')
011500121213     d wctcpa          c                   Const('CONS.PARTICOLARI aggiunte:')
011600121213     d wctcpm          c                   Const('CONS.PARTIC. Da:')
011700121213     d wccasa          c                   Const('C/A aggiunto:')
011800121213     d wccasm          c                   Const('C/A Modif. Da:')
011900121213     d wciasa          c                   Const('IMPORTO ASS. Aggiunto:')
012000121213     d wciasm          c                   Const('IMPORTO ASS. Modif. Da:')
012100140131     d wcric           c                   Const('Tassazione Evento "RIC"')
012200060714
012300060714
012400060714      *------------------------------------------------------------------------*
012500060714      *  RIEPILOGO INDICATORI
012600060714      *------------------------------------------------------------------------*
012700061005
012800121127      *       -
012900060714
013000060714      *------------------------------------------------------------------------*
013100060714
013200121127      * Blocco la data penultima fatturazione mensile su tabella "RPF"
013300121127     c                   exsr      blk_data
013400121127      * elaboro variazioni post-fatturazione
013500060717     c                   exsr      Elabora
013600060718
013700121227     c                   exsr      routend
013800060718
013900060717      *------------------------------------------------------------------------*
014000121127      * Blocco la data penultima fatturazione tabella "RPF"
014100060717      *------------------------------------------------------------------------*
014200121127     c     blk_data      BegSr
014300121127      *
014400121127     c* Determinazione della data da memorizzare in tabella:
014500121206     c* - Sempre il primo giorno del mese
014600121127     c* - Se data del giorno dopo il 15 mantengo il mese/anno corrente
014700121127     c*   Se data del giorno il 15 o prima il mese è il precedente al corrente
014800121127     c                   z-add     woggi         §rpfdta
014900121127     c                   move      01            §rpfdta
015000121127     c                   if        %subdt(dataiso_cor:*days) <=15
015100121127     c                   eval      dataiso=%date(§rpfdta:*iso)
015200121127     c                   subdur    1:*m          dataiso
015300121127     c                   eval      §rpfdta=%dec(dataiso:*iso)
015400121127     c                   Endif
015500121127     c*
015600121127     c                   eval      kcode = 'RPF'
015700121127     c                   eval      kke1 =  'DATA'
015800121218
015900121218     a* aggiorno solo se non sono in simulazione
016000121218     c                   if        w_simul=*blanks
016100121218
016200060717     c     Ktbe          chain     tntbe000
016300060717      *
016400121127     c                   If        %found
016500121227     c                   movel     §rpfdta       tbeuni
016600121127     c                   update    tntbe000
016700121127     c                   Endif
016800121218
016900121218     c                   endif
017000060717      *
017100121127     c                   endsr
017200060717      *------------------------------------------------------------------------*
017300121127      * Elaboro il file TIVRB contenente le variazioni da recuperare
017400060717      *------------------------------------------------------------------------*
017500060717     c     Elabora       BegSr
017600150721
017700150721     c                   Clear                   dsQt1
017800150721     c                   Movel     'QT'          tblcod
017900150721     c                   Movel(p)  1             tblkey
018000150721     c     ktab          Chain     Tabel00f
018100150721     c                   If        %Found(Tabel00f) and TblFlg = *Blanks
018200150721     c                   Movel     TblUni        dsQt1
018300150721     c                   EndIf
018400060717
018500121206     c     *loval        setll     tivrb01l
018600121127    1c                   do        *hival
018700060717
018800121221     c                   if        w_simul='S'
018900121221     c                   read(n)   tivrb01l
019000121221     c                   else
019100121206     c                   read      tivrb01l
019200121221     c                   endif
019300060717
019400121206    2c                   if        %eof(tivrb01l)
019500060717     c                   leave
019600121127    2c                   endif
019700121206
019800121206     c* Scarto record con data fattura > della data ultima fatturaz.
019900121206     c* (le fatture settimanali dell' ultimo mese verranno recuperate il
020000121206     c* mese successivo)
020100121206    2c                   if        vrbdft>§rpfdta
020200121127     c                   iter
020300121127    2c                   endif
020400121213     c* record con causale variazione blanks: memorizzo tracciato record
020500121213     c* (questo record non lo flaggo subito come elaborato ma viene flaggato
020600121213     c* insieme al record "VA" dopo che è stata creata la bolla di recupero)
020700121211    2c                   if        vrbcvb=*blanks
020800121210     c                   eval      ds_vrbor=ds_vrb
020900121206     c                   iter
021000121206     c                   endif
021100121206     c* Verifico esclusioni per cliente e area
021200121206     c                   exsr      Record_ok
021300121206     c* Se recupero non previsto per area o cliente leggo altro record
021400121206    2c                   If        Rec_ok <>' '
021500130109     c                   eval      wflg=Rec_ok
021600121206     c                   exsr      updvrb
021700121206     c                   iter
021800121206    2c                   endif
021900121206     c* Impostazione campi per la tassazione
022000121206     c                   exsr      tassaz
022100121206     c                   if        dtas_taserr=*blanks and
022200121207     c                             (dtas_tasimv-f_tasimv)>1
022300130327     c* prima di procedere alla creazione della bolla verifico che ci siano
022400130327     c* effettivamente delle variazioni fra i dati del record originale e
022500130327     c* i dati del record contenente le variazioni. Se non ci fossero differenze
022600130327     c* la maggior tassazione dipenderebbe esclusivamente da una modifica alla
022700130327     c* tariffa ed in questo caso non bisogna recuperare nulla
022800130327     c                   exsr      ver_var
022900130327     c                   if        wnoRA=*blanks
023000130327     c                   eval      wflg='='
023100130327     c                   exsr      updvrb
023200130327     c                   iter
023300130327     c                   else
023400121206     c                   exsr      creabolla
023500121206     c                   endif
023600130327     c                   endif
023700121213     c* flaggo tivrb come elaborato a prescindere dalla creazione o meno della bolla
023800121213     c* di recupero
023900121217     c                   clear                   wflg
024000121217     c                   select
024100121217     c                   when      dtas_taserr<>*blanks
024200121217     c                   eval      wflg='E'
024300121217     c                   when      dtas_tasimv-f_tasimv<=1
024400121217     c                   eval      wflg='<'
024500121217     c                   endsl
024600121213     c                   exsr      updvrb
024700060717
024800121127    1c                   enddo
024900121127
025000121127
025100060717     c                   endsr
025200060717      *------------------------------------------------------------------------*
025300121127      * Verifico se spedizione da elaborare
025400060717      *------------------------------------------------------------------------*
025500060717     c     Record_ok     BegSr
025600060717
025700060717     c                   clear                   rec_ok
025800121127     c     ktas          chain     titas30c
025900121127     c                   if        not %found(titas30c)
026000121127     c                   eval      rec_ok = 'N'
026100121211     c                   leavesr
026200121127     c                   endif
026300121207     c                   movel     f_tasksc      filksc
026400130109     c* Verifico filiale cliente se da elaborare
026500130109     c* Ignoro questo controllo se simulazione su tutte le aree
026600121127     c     filksc        chain     azorg01l
026700130325     c                   if        w_area=*blanks and w_tuttea<>'S'
026800121127     c                   if        not %found(azorg01l) or
026900130325     c                             %lookup(%char(orgcar):sare)=0
027000130325     c****                         (%lookup(%char(orgcar):sare)=0 and
027100130325     c****                         %lookup('999':sare)=0)
027200121127     c                   eval      rec_ok = 'N'
027300121211     c                   leavesr
027400121211     c                   endif
027500130109     c                   endif
027600121127     c
027700121127     c* verifico cliente         se da elaborare
027800061006      * verifico l'anagrafica
027900061006     c                   Clear                   Tibs69ds
028000121207     c                   Z-add     f_tasksc      i69kcs
028100121221     c                   Z-add     f_tasksc      i69kac
028200061006     c                   Call      'TIBS69R'
028300061006     c                   Parm                    Tibs69ds
028400061006     c                   Parm                    ds_cnaco
028500061006     c                   Parm                    ds_cnind
028600061006     c                   Parm                    ds_cnclp
028700061006     c                   Parm                    ds_fncls
028800121127     c                   If        o69err = ' '  and %subst(clsflo:3:1)='N'
028900130109     c                   eval      rec_ok = 'C'
029000121211     c                   leavesr
029100061006     c                   endif
029200121221     c                   eval      s_acorag=acorag
029300060717
029400121211     c                   endsr
029500121206      *------------------------------------------------------------------------*
029600121206      * Tassazione bolla
029700121206      *------------------------------------------------------------------------*
029800121206     c     Tassaz        BegSr
029900150330
030000150330     C                   MOVEL     '3A'          tblcod
030100150330     C                   MOVEL(P)  f_tascbo      tblkey
030200150330     c                   clear                   ds3a
030300150330     C     KTAB          CHAIN     TABEL00f
030400150330     c                   if        %found(tabel00f)
030500150330     C                   MOVEL     TBLUNI        DS3a
030600150330     c                   endif
030700150330
030800121206     c                   clear                   dtas
030900121206     c                   clear                   dtasv
031000121207     c                   eval      dtas_TASKSC=f_tasksc
031100121207     c                   eval      dtas_TASNCL=f_tasncl
031101160531     c                   eval      dtas_TASNCLB=f_tasncl
031200121207     c                   eval      dtas_TASCTR=f_tasctr
031300121207     c                   eval      dtas_TASFDN=f_tasfdn
031400121207     c                   eval      dtas_TASMCT=f_tasmct
031500121207     c                   eval      dtas_TASNZM=f_tasnzm
031600121207     c                   eval      dtas_TASCAM=f_tascam
031700121207     c                   eval      dtas_TASFAP=f_tasfap
031800121207     c                   eval      dtas_TASTSP=f_tastsp
031900121207     c                   eval      dtas_taslna=f_taslna
032000121207     c                   eval      dtas_TASDIV=f_tasdiv
032100121207     c                   eval      dtas_TASPVL=f_taspvl
032200121206     c                   eval      dtas_taspkf=VRBPKF
032300121206     c                   eval      dtas_taspkc=vRBPKC
032400121206     c                   eval      dtas_tasncp=VRBNCP
032500160603     C****               IF        §qtdst>0 and f_TASDFT<=§QTDST
032600150720     c* peso vdl: se spedizione fatturata prima della scadenza della tara
032700150720     c*           passo a tnsf20r il peso vdl a cui sottraggo la differenza di
032800150720     c*           tara fra quella vecchia e quella attuale moltiplicata per
032900150720     c*           il totale dei colli vdl
033000150720     c*           Serve per tassare con la stessa tara utilizzata al momento
033100150720     c*           della fatturazione
033200160603     C****               eval      dtas_taspkc=dtas_taspkc-((§qttco-§qttpc)*
033300160603     c****                                                   dtas_tasncp)
033400160603     C****               ENDIF
033500121206     c                   eval      dtas_tasvlf=VRBVLF
033501160531     c                   eval      dtas_tasfvf=VRBfvf
033502160603     c* Passo alla tassazione i dati del VDL solo dopo la fatturazione di luglio perchè
033503160603     c* il vdl che sostituisce il reale in tassazione entra in vigore solo a partire dalle bolle
033504160603     c* di giugno.  Se non ci fosse il test seguente il programma recupererebbe dei VDL sui reali
033505160603     c* anche sulle bolle di maggio
033506160603     c                   if        woggi  > 20160715
033507160531     c                   eval      dtas_tasvlc=VRBVLc
033508160531     c                   eval      dtas_tasncr=VRBncr
033509160603     c                   endif
033600121206     c                   eval      dtas_tascts=VRBCTS
033700121206     c                   eval      dtas_tasnzd=VRBNZD
033800121206     c                   eval      dtas_tascad=VRBCAD
033900121206     c                   eval      dtas_tasfin=VRBFIN
034000121206     c                   eval      dtas_tastc1=VRBTC1
034100121206     c                   eval      dtas_tastc2=VRBTC2
034200121206     c                   eval      dtas_taslnp=vrblnp
034300121206     c                   eval      dtas_TASTBL=vrbtbl
034400121206     c                   eval      dtas_tasvas=VRBVAS
034500121206     c                   eval      dtas_tasias=VRBIAS
034600121206     c                   eval      dtas_tasqft=VRBQFT
034700121206     c                   eval      dtas_tastic=VRBTIC
034800121206     c                   eval      dtas_tascas=VRBCAS
034900121206     c                   eval      dtas_tasvca=VRBVCA
035000121206     c                   eval      dtas_tascmb=VRBCMB
035100121206     c                   move      VRBSTA        dtas_tassta
035200140127     c                   eval      dtas_tasric=vrbric
035300121206     c                   clear                   dAr5Ban
035400121206     c                   Clear                   dAr5Bnb
035500121206     c                   Clear                   kpjbu
035600121207     c                   movel     f_tasaas      dtas_tasdsp
035700121207     c                   move      f_tasmgs      dtas_tasdsp
035800121206     c                   eval      dtas_tasdct=dtas_tasdsp
035900121206
036000121206      * Flag SI NO DDT
036100121207    2C                   If        f_tasll1 = 'C' or f_tasll1 = 'S'
036200121206     C                   movel     'S'           dtas_Tasddt
036300121206   x2C                   Else
036400121206     C                   Clear                   dtas_Tasddt
036500121206    2C                   Endif
036600121206     c                   clear                   dtas01
036700121207     c                   movel     f_tasfbr      §asfbr
036800121207     c                   movel     f_tascca      §ascca
036900121206      * valorizzo flag email al destinatario
037000121207     c                   movel     f_tasflo      dtasflo
037100121206     c                   move      §floemd       §asemd
037200150108     c* flag pin code
037300150108     c                   if        f_tasgma<>*blanks
037400150108     c                   clear                   ds7r
037500150108     c                   movel     '7R'          Tblcod
037600150108     c                   movel(p)  f_tasgma      Tblkey
037700150108     c     ktab          chain     tabel00f
037800150108    8c                   if        %found(tabel00f)
037900150108     c                   movel     tbluni        ds7r
038000150108    8c                   endif
038100150108     c                   if        §7rpincode='S'
038200150108     c                   movel     'S'           §aspin
038300150108     c                   endif
038400150108     c                   endif
038500121206
038600121206     c                   eval      dtas_tasflo = dtas01
038700160108     c* solo per prima bolla
038800160108     c                   if        §3atb2=*blanks or %subst(f_tastbl:1:1)
038900160108     c                             <>%subst(§3atb2:1:1)
039000160108     c
039100160108     c* flag DIRITTO DI CHIAMATA  da 10° byte di TASFLO di TITAS
039200160108     c                   move      §floado       dtas_tasprt
039300160108     c* flag per ADDEBITO PACKING LIST da fiar5 "GEN"
039400160108     c                   clear                   dar5gen
039500160108     c                   eval      kAr5Trd  = 'GEN'
039600160108     c     kFiar5        Chain     Fiar531c
039700160108If  3c                   If        %Found(Fiar531c)
039800160108     c                   Movel     Ar5Uni        dAr5gen
039900160108    3c                   EndIf
040000160108     c                   IF        §AR5als = 'S' or §AR5alx = 'S'
040100160108     c                   Eval      dtas_TasSpl = 'S'
040200160108     c                   endif
040300160108     c                   endif
040400121206
040500121206     c
040600121206      * Bancali
040700121207    2c                   If        %Subst(f_TasGva:1:1) = 'B'
040800121206
040900121206     c                   eval      kAr5Trd  = 'BAN'
041000121206     c     kFiar5        Chain     Fiar531c
041100121206If  3c                   If        %Found(Fiar531c)
041200121206     c                   Movel     Ar5Uni        dAr5Ban
041300121206    3c                   EndIf
041400121206      * Bancali
041500121206     c                   Eval      dtas_TasBan = §Ar5Nba + §Ar5Nb2
041600121206    2c                   EndIf
041700121206      **
041800121206      * Colli Originali
041900121207    2c                   If        %Subst(f_TasGva:1:1) = 'O'
042000121206     c                   eval      kAr5Trd  = 'BNB'
042100121206     c     kFiar5        Chain     Fiar531c
042200121206If  3c                   If        %Found(Fiar531c)
042300121206     c                   Movel     Ar5Uni        dAr5Bnb
042400121206    3c                   EndIf
042500121206      * Colli
042600121206     c                   Eval      dtas_TasNcl = §Ar5bcor
042700121206    2c                   EndIf
042800121206
042900121206      **** verifico se devo calcolare l'addebito di lasciato avviso
043000121206      **** verifico se c'è evento
043100140127     c***  Kevb4         chain     Fnevb04l
043200121206
043300140127    2c***                If        %found(Fnevb04l)
043400140127     c***                eval      dtas_tasric = 'S'
043500140127    2c***                endif
043600121206
043700121206      * Assicurazione
043800121207    2c                   IF        f_Tasfcm = 'F'
043900121206     c                   clear                   dtas_tasias
044000121206     c                   clear                   dtas_tasvas
044100121206    2c                   Endif
044200121206      * recupero i valori  già tassati
044300121206     c                   exsr      Ric_tai
044400150330
044500150330     c* se devo tassare solo una varia, da tipo bolla, la imposto
044600150330     c                   if        §3asva<>*blanks
044700150330     c                   eval      dtas_tassva=§3asva
044800150330     c                   endif
044900121206
045000121206     C                   CALL      'TNSF20R'
045100121206     C                   PARM                    KPJBA
045200121206     C                   PARM                    DTAS
045300121206     C                   PARM                    DTASV
045400121206
045500150330     c                   endsr
045600121206      *****************************************************************
045700121206      *  Recupero importi di tassazione dal file titai
045800121206      *****************************************************************
045900121206     c     Ric_tai       Begsr
046000121206      *
046100121206     c                   z-add     0             k
046200121206
046300121206     c     ktai05        setll     titai01l
046400121206
046500121206     c                   If        %equal(titai01l)
046600121206
046700121206     c                   do        *hival
046800121206
046900121206     c     ktai05        reade     titai01l
047000121206
047100121206     c                   If        %eof(titai01l)
047200121206     c                   leave
047300121206     c                   endif
047400121206      * imponibile
047500121206     c                   if        taisvt = '£'
047600121206     c                   eval      dtas_tasimv = taivat
047700121206     c                   endif
047800121206      * porto
047900121206     c                   if        taisvt = ' '
048000121206     c                   eval      dtas_taspor = taivat
048100121206     c                   endif
048200121206      * varie
048300121206     c                   if        taisvt <> ' ' and taisvt <> '£'
048400121206     c* se presente varia N 88.888.888 devo prima tassarla
048500121206    4c                   if        taisvt='N' and taivat=88888888
048600121206     c                   eval      savdtas=dtas
048700121206     c                   eval      savdtasv=dtasv
048800121206     c                   exsr      tassan
048900121206     c                   eval      dtas=savdtas
049000121206     c                   eval      dtasv=savdtasv
049100121206     c                   add       1             k
049200121206     c                   eval      sv(k) ='N'
049300121206     c                   eval      va(k) =antepo
049400121206   x4c                   else
049500121206     c                   add       1             k
049600121206     c                   eval      sv(k) = taisvt
049700121206     c                   eval      va(k) = taivat
049800121206    4c                   endif
049900121206     c                   endif
050000121206
050100121206     c                   enddo
050200121206
050300121206     c                   else
050400121206      * se non è pretassata pulisco la divisa
050500121206     c                   clear                   dtas_tasdiv
050600121206     c                   endif
050700121206     c                   endsr
050800121206      ****************************************************
050900121206      ** Tassazione varia N 88888888 (Anteporto)
051000121206      ****************************************************
051100121206     c     tassan        Begsr
051200121206     C                   CLEAR                   DTAS_tasimv
051300121206     C                   CLEAR                   DTAS_taspor
051400121206     C                   CLEAR                   DTASV
051500121206     c                   clear                   antepo
051600121206      * tolgo il flag dell'invio mail al destinatario nell'eventualità ci fosse
051700150108      * idem per Pin Code
051800160113      *                 e per diritto di chiamata e packing list
051900121206     c                   eval      dtas01 = dtas_tasflo
052000121206     c                   clear                   §asemd
052100150108     c                   clear                   §aspin
052200121206     c                   eval      dtas_tasflo = dtas01
052300160113     c                   clear                   dtas_tasprt
052400160113     c                   clear                   dtas_tasspl
052500121206      *
052600121206     C                   CALL      'TNSF20R'
052700121206     C                   PARM                    KPJBA
052800121206     C                   PARM                    DTAS
052900121206     C                   PARM                    DTASV
053000121206      *
053100121206      * Tassazione andata buon fine
053200121206     C     dtas_taserr   IFEQ      *BLANKS
053300121206     c                   eval      antepo=dtas_tasimv
053400121206     C                   ENDIF
053500121206     c                   endsr
053600121206      *------------------------------------------------------------------------*
053700121206      * Creazione bolla di recupero
053800121206      *------------------------------------------------------------------------*
053900121206     c     Creabolla     BegSr
054000121206
054100151016     c* Chaino Fiar5 record "FAT" e se c'è lo utilizzo per memorizzarlo anche
054200151016     c* sulla bolla di recupero anche nella memorizzazione di pkf e pkb
054300151026     c                   clear                   dar5fat
054400151026     c                   eval      kAr5Trd  = 'FAT'
054500151026     c     kFiar5        Chain     Fiar531c
054600151026If  3c                   If        %Found(Fiar531c)
054700151026     c                   eval      dar5fat=ar5uni
054800151026     c                   endif
054900121206
055000121206      * -- > T I T A S 0 0 0
055100121206      *---------------------
055200121206     c                   Clear                   Titas0
055300121206      * Anno
055400121211     c                   movel     §rpfdta       tasaas
055500121206      * Mese/giorno
055600121211     c                   movel     tasaas        tasdsp            8 0
055700121211     c                   move      §rpfdta       tasdsp
055800121207     c                   eval      dataiso=%date(tasdsp:*iso)
055900121206     c                   adddur    1:*m          dataiso
056000121207     c                   subdur    1:*d          dataiso
056100121207     c                   eval      tasdsp=%dec(dataiso)
056200121207
056300121207     c                   move      tasdsp        tasmgs
056400121207     c
056500121206      * Linea di partenza
056600121207     c                   Eval      taslnp=f_taslnp
056700121206
056800121221      * Codice bolla
056900121211     c                   Eval      TasCbo = 'BV'
057000121206      * Tipo bolla
057100121213     c                   Eval      TasTbl = s_§3atb1
057200121206      * codice cliente
057300121207     c                   Eval      tasksc = f_tasksc
057400121207      * cliente mittente
057500121207     c                   eval      Tasccm = f_tasksc
057600121207      * Linea di arrivo
057700121207     c                   Eval      taslna = f_TasLna
057800121206      * Numero Colli
057900121207     c                   Eval      TasNcl = F_TasNcl
058000151016     c* impostazione campi Peso se non presente il record FAT
058100151026     c                   if        §AR5PKTAS=0
058200121212      * Peso bollettato  = Peso Fatturato
058300150721      * il peso che memorizziamo è quello usato sulla bolla originale per
058400150721      * fatturare ovvero taspkf= V --> vdl - tara (taspkc)
058500150721      *                  taspkf<>V --> peso boll/variato (taspkf)
058600121213     c                   if        f_tasfpf<>'V'
058700121212     c                   Eval      TasPkb = F_TasPkf
058800121212     c                   Eval      tasPkf = f_TasPkf
058900121212     c                   else
059000150721     C                   IF        f_tASDFT<=§QTDST
059100150721     c     §QtTco        Mult      f_tasncp      wTara
059200150720     c                   else
059300121212     c     §QtTpc        Mult      f_tasncp      wTara
059400150721     c                   endif
059500121212     c     f_taspkc      Sub(h)    wTara         wNtara
059600121212     c                   Z-Add     wNtara        taspkb
059700121212     c                   Z-Add     wNtara        taspkf
059800121212     c                   endif
059900151016     c                   else
060000151016     c* Se presente il record "FAT"  prendo i pesi da titas della bolla "mamma" così
060100151016     c* come sono
060200151016     c                   Eval      TasPkb = F_TasPkb
060300151016     c                   Eval      tasPkf = f_TasPkf
060400151016     c                   Eval      tasPkc = f_TasPkc
060500151016     c                   Eval      tasNcp = f_TasNcp
060600151016     c                   endif
060700121207      * Volume bollettato + flag = Volume fatturato
060800121212     c                   Eval      TasVlb = F_Tasvlf
060900121212     c                   Eval      TasFvb = f_tasfvf
061000121212     c                   Eval      TasVlf = f_tasvlf
061100121212     c                   Eval      TasFvf = f_tasfvf
061200121207      * codice tariffa
061300121207     c                   Eval      tasctr=f_tasctr
061400121206      * Tipo servizio
061500121207     c                   Eval      TasTsp = F_tastsp
061600121207     c* determino se bolla originale è franco o assegnato
061700121207     c                   exsr      rep_dstb
061800121206      * Destinatario
061900121207      * -->  mittente o destinatario della bolla originale in base al porto
062000121207     c                   if        §tbtpo='F'
062100121207     c                   Eval      TasRsd = f_TasRsd
062200121207     c                   Eval      TasInd = f_TasInd
062300121207     c                   Eval      TasCad = f_TasCad
062400121207     c                   Eval      TasLod = f_TasLod
062500121207     c                   Eval      TasPrd = f_TasPrd
062600121207     c                   Eval      TasNzd = f_TasNzd
062700121207     c                   Eval      TasCpd = f_TasCpd
062800121207     c                   Eval      Tascts = f_tascts
062900121207     c                   Eval      Tasmct = f_tasmct
063000121207     c                   Eval      Tasfin = f_tasfin
063100121207     c                   Eval      Tasfap = f_tasfap
063200121207     c                   else
063300121211     c                   move      f_tasccm      wccm              4 0
063400121207     c* mittente bolla originale codificato
063500121207     c                   if        wccm<>8888
063600121207     c                   Clear                   Tibs69ds
063700121207     c                   Z-add     f_tasccm      i69kac
063800121207     c                   Z-add     f_tasccm      i69kin
063900121207     c                   Call      'TIBS69R'
064000121207     c                   Parm                    Tibs69ds
064100121207     c                   Parm                    ds_cnaco
064200121207     c                   Parm                    ds_cnind
064300121207     c                   Parm                    ds_cnclp
064400121207     c                   Parm                    ds_fncls
064500121207     c                   Eval      TasRsd = acorag
064600121207     c                   Eval      TasInd = indvia
064700121207     c                   Eval      TasCad = indCae
064800121207     c                   Eval      TasLod = indcit
064900121211     c                   Eval      TasPrd = indPrv
065000121207     c                   Eval      TasNzd = indsta
065100121207     c                   Eval      TasCpd = indiva
065200121207     c                   else
065300121207     c* cliente non codificato prendo dati da titaa
065400121207     c                   eval      ktrc = 'M'
065500121207     c     KTAStrc       CHAIN     TITAA30C
065600121207     c                   IF        %FOUND
065700121207     c                   movel     TAArsc        tasrsd
065800121207     c                   movel     TAAind        tasind
065900121207     c                   movel     TAAloc        taslod
066000121207     c                   movel     TAAnaz        tasnzd
066100121207     c                   movel     TAAprv        tasprd
066200121207     c                   movel     TAAcap        tascad
066300121207     c                   movel     TAAcpi        tascpd
066400121207     c                   ENDIF
066500121207     c                   endif
066600121207     c                   Eval      Tascts = f_tasmct
066700121207     c                   Eval      Tasmct = f_tascts
066800121207     c                   Eval      Tasfin = f_tasfap
066900121207     c                   Eval      Tasfap = f_tasfin
067000121207     c                   endif
067100121207      * Riferimento mittente numerico
067200121207     c                   z-add     f_TasRmn      TasRmn
067300121206      * Natura merce
067400160412     c***                Eval      tasNas = f_tasnas
067500121206      * P.o. bollettazione
067600121207     c                   Eval      TasFlb = Taslnp
067700121206      * Terminal Arrivo
067800121207     c                   Eval      TasTfa = F_TasTfa
067900121207     c                   clear                   fnlv55ds
068000121207     c                   eval      d55tpt='A'
068100121207     c                   eval      d55lin=taslna
068200121207     c                   eval      d55drf=*date
068300121207     c                   call      'FNLV55R'
068400121207     c                   parm                    fnlv55ds
068500121207     c                   if        d55err<>*blanks
068600121207     c                   leavesr
068700121207     c                   endif
068800121207     c                   eval      tastfa=d55tfa
068900121206      * Terminal partenza
069000121207     c                   clear                   fnlv55ds
069100121207     c                   eval      d55tpt='P'
069200121207     c                   eval      d55lin=taslnp
069300121207     c                   eval      d55drf=*date
069400121207     c                   call      'FNLV55R'
069500121207     c                   parm                    fnlv55ds
069600121207     c                   if        d55err<>*blanks
069700121207     c                   leavesr
069800121207     c                   endif
069900121207     c                   eval      tastfp=d55tfp
070000121206      * Data immissione
070100121207     c                   Eval      tasdim=woggi
070200121221      * Sigla operatore
070300121206     c                   Eval      TasSop = '*'
070400121206      * Codice autotrasportatore
070500121206     c                   Movel     TasLnp        TasPdr
070600121206     c                   Move      999           TasPdr
070700121206      * Data     ritiro
070800121206     c                   Eval      TasDrt = (tasaas * 10000) + TasMgs
070900121206      * Flag prestazione autotrasportatore
071000121206     c                   Eval      TasFpp = 'P'
071100121206      * Trattamento merce
071200121213     c                   Eval      TasCtm = s_§3actm
071300121206      * Data Borderò
071400121206     c                   Eval      TasDbr = (tasaas * 10000) + TasMgs
071500121206      * Flag merce portata a magazzino
071600121206     c                   Eval      TasFdn = 'S'
071700121206      * Flag ddt si/no
071800121206     c                   Eval      TasLl1 = 'Y'
071900121206      * Nazione e cap mittente
072000121206     c                   Clear                   Tibs69ds
072100121206     c                   Z-add     TasKsc        i69kin
072200121206     c                   Call      'TIBS69R'
072300121206     c                   Parm                    Tibs69ds
072400121206     c                   Parm                    ds_cnaco
072500121206     c                   Parm                    ds_cnind
072600121206     c                   Parm                    ds_cnclp
072700121206     c                   Parm                    ds_fncls
072800121206     c                   If        o69err = '1'
072900121207     c                   leavesr
073000121206     c                   EndIF
073100121206     c                   Eval      TasNzm = IndSta
073200121206     c                   Eval      TasCam = IndCae
073300121206      * divisa
073400121206     c                   Eval      TasDiv = 'EUR'
073500121207     c* Trasporto
073600121207     c                   Eval      Taspor = dtas_tasimv-f_tasimv
073700121207     c                   Eval      Tasimv = taspor
073800121206      * Falg bartolini/sdi
073900121206     c                   Eval      TasScl = 'B'
074000121206      * Numero spedizione (da Flnuf cod. 3)
074100121218     c                   if        w_simul=*blanks
074200121206     c                   Clear                   Trul34ds
074300121206     c                   Eval      i34Aas = TasAas
074400121206     c                   Eval      i34Lnp = TasLnp
074500121206     c                   Call      'TRUL34R'
074600121206     c                   Parm                    kpjba
074700121206     c                   Parm                    Trul34ds
074800121206     c                   If        f_o34err <> *Zeros
074900121207     c                   leavesr
075000121206     c                   EndIF
075100121218     c                   else
075200121218     c* In simulazione non stacco numero da flnuf ma utilizzo contatore interno partendo sempre
075300121218     c* dal numero 5000001
075400121218     c                   eval      w_contnum+=1
075500121218     c                   eval      o34nsp=w_contnum
075600121218     c                   endif
075700121206     c                   Eval      TasNsp = o34Nsp
075800121210
075900121210     c*----------------
076000121210     c* T I T A 4 0 0 0
076100121210     c*----------------
076200121210     c* creo record note per memorizzare il nuemro della bolla originale
076300121210     c                   clear                   tita40
076400121210     c                   eval      ta4aas=tasaas
076500121210     c                   eval      ta4lnp=taslnp
076600121210     c                   eval      ta4nrs=tasnrs
076700121210     c                   eval      ta4nsp=tasnsp
076800121210     c                   eval      ta4trc='8'
076900121210     c                   eval      ta4not='Bolla originale ' +
077000121210     c                             %editc(f_taslnp:'X')
077100121210     c                   if        f_tasnrs>0
077200121210     c                   eval      ta4not=%trim(ta4not) + ' ' +
077300121210     c                             %editc(f_tasnrs:'X') + '-' +
077400121213     c                             %trim(%editc(f_tasnsp:'4'))
077500121210     c                   else
077600121210     c                   eval      ta4not=%trim(ta4not) + ' ' +
077700121213     c                             %trim(%editc(f_tasnsp:'4'))
077800121210     c                   endif
077900121210     c                   eval      ta4not=%trim(ta4not) + ' ' +
078000121210     c                             %editc(f_tasaas:'X')
078100121213     c                   Write     Tita40
078200160122      * Riferimento mittente alfanumerico  e natura merce
078300121211     c                   clear                   s_§ta4arma
078400160122     c                   clear                   s_§ta4anas
078500121211     c                   eval      ktrc='A'
078600121211     c     ktastrc       chain     tita430c
078700121211     c                   if        %found(tita430c)
078800121211     c                   movel     f_ta4not      dta4a
078900121211     c                   movel     §ta4arma      s_§ta4arma
079000160122     c                   movel     §ta4anas      s_§ta4anas
079100121211     c                   endif
079200160122     c*
079300160122     c***                if        s_§ta4arma<>*blanks
079400121211     c                   clear                   tita40
079500121211     c                   eval      ta4aas=tasaas
079600121211     c                   eval      ta4lnp=taslnp
079700121211     c                   eval      ta4nrs=tasnrs
079800121211     c                   eval      ta4nsp=tasnsp
079900121211     c                   eval      ta4trc='A'
080000121211     c                   clear                   dta4a
080100121211     c                   eval      §TA4ARMA=s_§ta4arma
080200160122     c                   eval      §TA4Anas=s_§ta4anas
080300121211     c                   eval      ta4not=dta4a
080400121213     c                   Write     Tita40
080500160122     c***                endif
080600121210     c*----------------
080700121210     c* F I A R 5 0 0 0
080800121210     c*----------------
080900121210     c* creo su fiar5 legame bolla originale --> bolla di recupero
081000121210     c* e viceversa
081100121210     c* legame bolla di recupero --> bolla originale
081200121210     C                   clear                   fiar50
081300121210     C                   eval      ar5aas=tasaas
081400121210     C                   eval      ar5lnp=taslnp
081500121210     C                   eval      ar5nrs=tasnrs
081600121210     C                   eval      ar5nsp=tasnsp
081700121221     c*
081800121210     C                   eval      ar5trd='LEG'
081900121210     C                   eval      ar5uni=%editc(f_tasaas:'X') +
082000121210     c                             %editc(f_taslnp:'X') +
082100121210     c                             %editc(f_tasnrs:'X') +
082200121210     c                             %editc(f_tasnsp:'X')
082300121210     c                   eval      ar5dac=woggi
082400121210     c                   time                    ar5orc
082500121210     c                   write     fiar50
082600121210     c* legame bolla originale  --> bolla di recupero
082700121210     C                   clear                   fiar50
082800121210     C                   eval      ar5aas=f_tasaas
082900121210     C                   eval      ar5lnp=f_taslnp
083000121210     C                   eval      ar5nrs=f_tasnrs
083100121210     C                   eval      ar5nsp=f_tasnsp
083200121221     c*
083300121210     C                   eval      ar5trd='LEG'
083400121210     C                   eval      ar5uni=%editc(tasaas:'X') +
083500121210     c                             %editc(taslnp:'X') +
083600121210     c                             %editc(tasnrs:'X') +
083700121210     c                             %editc(tasnsp:'X')
083800121210     c                   eval      ar5dac=woggi
083900121210     c                   time                    ar5orc
084000121210     c                   write     fiar50
084100121210     c
084200151016     c* Creazione record "FAT" anche sulla bolla di recupero
084300151026     C                   if        §AR5PKTAS>0
084400151026     C                   clear                   fiar50
084500151026     C                   eval      ar5aas=tasaas
084600151026     C                   eval      ar5lnp=taslnp
084700151026     C                   eval      ar5nrs=tasnrs
084800151026     C                   eval      ar5nsp=tasnsp
084900151026     c*
085000151026     C                   eval      ar5trd='FAT'
085100151026     C                   EVAL      AR5UNI=DAR5FAT
085200151026     c                   eval      ar5dac=woggi
085300151026     c                   time                    ar5orc
085400151026     c                   write     fiar50
085500151026     C                   ENDIF
085600121210
085700121213     c                   Write     Titas0
085800121212
085900121212     c                   exsr      creaRA
086000121206
086100121207     c                   endsr
086200121206      *------------------------------------------------------------------------*
086300121206      * Creazione Richiesta Assistenza
086400121206      *------------------------------------------------------------------------*
086500121206     c     CreaRA        BegSr
086600140507     c                   clear                   fidna6ds
086700140507     c                   eval      ida06mad=' 30'
086800140507     c                   eval      ida06tor='S'
086900140507     c                   eval      ida06ogg=%editc(taslnp:'X')+
087000140507     c                             %editc(tasnrs:'X')+
087100121210     c                             %editc(tasnsp:'X')+%editc(tasaas:'X')
087200121210     c* verifica delle variazioni effettuate ed impostazione campo note da passare a pgm
087300130327     c* spostato il richiamo
087400130327     c*****              exsr      ver_var
087500140507     c                   eval      ida06no1=wnoRa
087600121218
087700121218     c* Crea richiesta assistenza solo se non in simulazione
087800121218     c                   if        w_simul=*blanks
087900121218
088000140507     c                   eval      ida06ute='EDPFAT    '
088100140507     c                   eval      ida06fil=dutpou
088200140507     c                   call      'FIDNA6R'
088300140507     c                   parm                    fidna6ds
088400121221     c                   endif
088500121218
088600121221     c* scrivo work_file
088700121221     c                   exsr      wrt_wf
088800121218
088900121218
089000121206     c                   endsr
089100121210      *------------------------------------------------------------------------*
089200121210      * Verifica variazioni effettuate
089300121210      *------------------------------------------------------------------------*
089400121210     c     ver_var       BegSr
089500121211     c                   clear                   wnoRa
089600130304     c* peso vdl o peso pkf in base a flag da tnsf20r
089700130304     c                   if        dtas_tasspc='S'
089800130304     c* peso VDL
089900130304     c                   if        or_vrbpkc<>vrbpkc
090000130304     c                   eval      wnoRa=wcpeso +
090100130304     c                             %trim(%editc(vrbpkc-or_vrbpkc:'4'))
090200130304     c                   eval      wnoRa=%trim(wnoRa) + ' Da VDL'
090300130304     c                   endif
090400130304     c                   else
090500130304     c* peso pkf
090600121210     c                   if        or_vrbpkf<>vrbpkf
090700121213     c                   eval      wnoRa=wcpeso +
090800121213     c                             %trim(%editc(vrbpkf-or_vrbpkf:'4'))
090900121210     c                   endif
091000130304     c                   endif
091100121210     c* volume
091200160603    1c***                if        or_vrbvlf<>vrbvlf
091201160603    1c                   if        or_vrbvlf<>dtas_tasvlt
091300121213    2c                   if        wnoRA<>*blanks
091400121212     c                   exsr      ver_pos
091500121213    3c                   if        %subst(wnora:279:2)='++'
091600121212     c                   leavesr
091700121213    3c                   endif
091800121213     c                   eval      %subst(wnoRa:Pos)=wcvolu
091900121213   x2c                   else
092000121213     c                   eval      wnoRa=wcvolu
092100121213    2c                   endif
092200121211     c                   eval      wnoRa=%trim(wnoRa) + ' ' +
092300160603     c                             %trim(%editc(dtas_tasvlt-or_vrbvlf:'4'))
092301160603     c***                          %trim(%editc(vrbvlf-or_vrbvlf:'4'))
092400130301     c* se da Vdl lo scrivo
092500160603     c****               if        vrbfvf='Z' or vrbfvf='T'
092501160603     c                   if        dtas_tasfvt='Z' or dtas_tasfvt='T'
092600130301     c                   eval      wnoRa=%trim(wnoRa) + ' Da VDL'
092700130301     c                   endif
092800121213    1c                   endif
092900121211     c* Nazione destinatario
093000121211     c                   if        or_vrbnzd<>vrbnzd
093100121211     c                   if        wnoRA<>*blanks
093200121212     c                   exsr      ver_pos
093300121212     c                   if        %subst(wnora:279:2)='++'
093400121212     c                   leavesr
093500121212     c                   endif
093600121213     c                   eval      %subst(wnoRa:Pos)=wcnzd
093700121211     c                   else
093800121213     c                   eval      wnoRa=wcnzd
093900121211     c                   endif
094000121211     c                   eval      wnoRa=%trim(wnora) + ' ' +
094100121213     c                             or_vrbnzd + ' A: ' + vrbnzd
094200121211     c                   endif
094300121211     c* Cap destinatario
094400121211     c                   if        or_vrbcad<>vrbcad
094500121211     c                   if        wnoRA<>*blanks
094600121212     c                   exsr      ver_pos
094700121212     c                   if        %subst(wnora:279:2)='++'
094800121212     c                   leavesr
094900121212     c                   endif
095000121212     c                   eval      %subst(wnoRa:Pos)=
095100121213     c                             wccad
095200121211     c                   else
095300121213     c                   eval      wnoRa=wccad
095400121211     c                   endif
095500121211     c                   eval      wnoRa=%trim(wnora) + ' ' +
095600121213     c                             %trim(or_vrbcad) + ' A: ' +
095700121212     c                             vrbcad
095800121211     c                   endif
095900121210     c* Cod.tassazione
096000121210     c                   if        or_vrbcts<>vrbcts
096100121211     c                   if        wnoRA<>*blanks
096200121212     c                   exsr      ver_pos
096300121212     c                   if        %subst(wnora:279:2)='++'
096400121212     c                   leavesr
096500121212     c                   endif
096600121212     c                   eval      %subst(wnoRa:Pos)=
096700121213     c                             wccts
096800121211     c                   else
096900121213     c                   eval      wnoRa=wccts
097000121211     c                   endif
097100121211     c                   eval      wnoRa=%trim(wnora) + ' ' +
097200121213     c                             or_vrbcts + ' A: ' + vrbcts
097300121210     c                   endif
097400121211     c* Fermo deposito
097500121211     c                   if        or_vrbffd<>vrbffd
097600121211     c                   if        or_vrbffd<>*blanks and vrbffd=*blanks
097700121211     c                   if        wnoRA<>*blanks
097800121212     c                   exsr      ver_pos
097900121212     c                   if        %subst(wnora:279:2)='++'
098000121212     c                   leavesr
098100121212     c                   endif
098200121212     c                   eval      %subst(wnoRa:Pos)=
098300121213     c                             wcffdt
098400121211     c                   else
098500121213     c                   eval      wnoRa=wcffdt
098600121211     c                   endif
098700121211     C                   else
098800121211     c                   if        wnoRA<>*blanks
098900121212     c                   exsr      ver_pos
099000121212     c                   if        %subst(wnora:279:2)='++'
099100121212     c                   leavesr
099200121212     c                   endif
099300121212     c                   eval      %subst(wnoRa:Pos)=
099400121213     c                             wcffda
099500121211     c                   else
099600121213     c                   eval      wnoRa=wcffda
099700121211     c                   endif
099800121211     c                   endif
099900121211     c                   endif
100000121210     c* Flag inoltro
100100121210     c                   if        or_vrbfin<>vrbfin
100200121211     c                   if        wnoRA<>*blanks
100300121212     c                   exsr      ver_pos
100400121212     c                   if        %subst(wnora:279:2)='++'
100500121212     c                   leavesr
100600121212     c                   endif
100700121212     c                   eval      %subst(wnoRa:Pos)=
100800121213     c                             wcfin
100900121211     c                   else
101000121213     c                   eval      wnoRa=wcfin
101100121211     c                   endif
101200121213     c                   eval      wnoRa=%trim(wnora) + ' "' +
101300121213     c                             or_vrbfin + '" a: ' + '"' + vrbfin
101400121213     c                             + '"'
101500121210     c                   endif
101600121210     c* Consegne particolari
101700121213    1c                   if        (or_vrbtc1=vrbtc1   and
101800121210     c                             or_vrbtc2=vrbtc2) or
101900121210     c                             (or_vrbtc1=vrbtc2   and
102000121210     c                             or_vrbtc2=vrbtc1)
102100121213   x1c                   else
102200121213    2c                   select
102300121213     c* . . . . . . Aggiunte
102400121213     c                   when      or_vrbtc1=*blanks and or_vrbtc2=*blanks
102500121213    3c                   if        wnoRA<>*blanks
102600121213     c                   exsr      ver_pos
102700121213    4c                   if        %subst(wnora:279:2)='++'
102800121213     c                   leavesr
102900121213    4c                   endif
103000121213     c                   eval      %subst(wnoRa:Pos)=
103100121213     c                             wctcpa
103200121213   x3c                   else
103300121213     c                   eval      wnoRa=wctcpa
103400121213    3c                   endif
103500121213     c                   if        vrbtc1<>*blanks
103600121213     c                   eval      wnoRa=%trim(wnora) + ' "' +
103700121213     c                             vrbtc1 + '"'
103800121213     c                   endif
103900121213     c                   if        vrbtc2<>*blanks
104000121213     c                   eval      wnoRa=%trim(wnora) + ' "' +
104100121213     c                             vrbtc2 + '"'
104200121213     c                   endif
104300121213     c                   other
104400121213     c* . . . . . . Modificate
104500121213    3c                   if        wnoRA<>*blanks
104600121212     c                   exsr      ver_pos
104700121213    4c                   if        %subst(wnora:279:2)='++'
104800121212     c                   leavesr
104900121213    4c                   endif
105000121212     c                   eval      %subst(wnoRa:Pos)=
105100121213     c                             wctcpm
105200121213   x3c                   else
105300121213     c                   eval      wnoRa=wctcpm
105400121213    3c                   endif
105500121213     c                   if        or_vrbtc1<>*blanks
105600121213     c                   eval      wnoRa=%trim(wnora) + ' "' +
105700121213     c                             or_vrbtc1 + '"'
105800121213     c                   endif
105900121213     c                   if        or_vrbtc2<>*blanks
106000121213     c                   eval      wnoRa=%trim(wnora) + ' "' +
106100121213     c                             or_vrbtc2 + '"'
106200121213     c                   endif
106300121213     c                   eval      wnoRa=%trim(wnora) +
106400121213     c                             ' a:'
106500121213     c                   if        vrbtc1<>*blanks
106600121213     c                   eval      wnoRa=%trim(wnora) + ' "' +
106700121213     c                             vrbtc1 + '"'
106800121213     c                   endif
106900121213     c                   if        vrbtc2<>*blanks
107000121213     c                   eval      wnoRa=%trim(wnora) + ' "' +
107100121213     c                             vrbtc2 + '"'
107200121213     c                   endif
107300121213    2c                   endsl
107400121213    1c                   endif
107500121210     c* Contrassegno
107600121210     c                   if        Or_vrbcas<>vrbcas or
107700121210     c                             Or_vrbtic<>vrbtic or
107800121210     c                             Or_vrbvca<>vrbvca or
107900121211     c                             Or_vrbcmb<>vrbcmb or
108000121211     c                             (Or_vrbsta<>vrbsta and vrbsta<>9)
108100121211     c                   select
108200121210     c* . . . . Contrassegno annullato
108300121211     c***                when      Or_vrbsta<>9 and vrbsta=9
108400121211     c***                eval      wnoRa='Contrassegno ANNULLATO'
108500121211     c* . . . . Contrassegno aggiunto
108600121211     c                   when      or_vrbcas= 0 and vrbcas>0
108700121211     c                   if        wnora<>*blanks
108800121212     c                   exsr      ver_pos
108900121212     c                   if        %subst(wnora:279:2)='++'
109000121212     c                   leavesr
109100121212     c                   endif
109200121212     c                   eval      %subst(wnoRa:Pos)=
109300121213     c                             wccasa
109400121211     c                   else
109500121213     c                   eval      wnoRa=wccasa
109600121211     c                   endif
109700121211     c                   eval      wnoRA=%trim(wnora) + ' '  +
109800121211     c                             vrbvca + ' ' +
109900121212     c                             %trim(%editc(vrbcas:'4'))
110000121211     c                   if        vrbtic = ' M'
110100121212     c                   eval      wnoRA=%trim(wnora) + ' Mittente'
110200121211     c                   else
110300121212     c                   eval      wnoRA=%trim(wnora) + ' Vettore'
110400121211     c                   endif
110500121211     c* . . . . Contrassegno modificato
110600121211     c                   other
110700121211     c                   if        wnora<>*blanks
110800121212     c                   exsr      ver_pos
110900121212     c                   if        %subst(wnora:279:2)='++'
111000121212     c                   leavesr
111100121212     c                   endif
111200121212     c                   eval      %subst(wnoRa:Pos)=
111300121213     c                             wccasm
111400121211     c                   else
111500121213     c                   eval      wnoRa=wccasm
111600121211     c                   endif
111700121211     c                   eval      wnoRA=%trim(wnora) + ' '  +
111800121212     c                             Or_vrbvca + ' ' +
111900121212     c                             %trim(%editc(or_vrbcas:'4'))
112000121211     c                   if        Or_vrbtic = ' M'
112100121212     c                   eval      wnoRA=%trim(wnoRa) + ' Mittente'
112200121211     c                   else
112300121212     c                   eval      wnoRA=%trim(wnoRa) + ' Vettore'
112400121211     c                   endif
112500121213     c                   eval      wnoRA=%trim(wnoRa) + ' A: ' +
112600121212     c                             vrbvca + ' ' +
112700121212     c                             %trim(%editc(vrbcas:'4'))
112800121211     c                   if        vrbtic = ' M'
112900121212     c                   eval      wnoRA=%trim(wnoRa) + ' Mittente'
113000121211     c                   else
113100121212     c                   eval      wnoRA=%trim(wnoRa) + ' Vettore'
113200121211     c                   endif
113300121211     c                   endsl
113400121211     c* . . . . . Scrivo se poi annullato
113500121211     c                   if        vrbsta=9
113600121211     c                   eval      wnoRA=%trim(wnora) + ' poi annullato'
113700121211     c                   endif
113800121211     c                   endif
113900121210     c* Importo da assicurare
114000121211     c                   if        Or_vrbvas<>vrbvas or
114100121211     c                             Or_vrbias<>vrbias
114200121211     c                   select
114300121211     c* . . . . . importo da assicurare aggiunto
114400121211     c                   when      or_vrbias=0
114500121212     c                   if        wnora<>*blanks
114600121212     c                   exsr      ver_pos
114700121212     c                   if        %subst(wnora:279:2)='++'
114800121212     c                   leavesr
114900121212     c                   endif
115000121212     c                   eval      %subst(wnoRa:Pos)=
115100121213     c                             wciasa
115200121211     c                   else
115300121213     c                   eval      wnoRa=wciasa
115400121211     c                   endif
115500121211     c                   eval      wnoRA=%trim(wnora) + ' '  +
115600121211     c                             vrbvas + ' ' + %editc(vrbias:'4')
115700121211     c
115800121211     c* . . . . . . importo da assicurare modificato
115900121211     c                   when      vrbias>0
116000121211     c                   if        wnora<>*blanks
116100121212     c                   exsr      ver_pos
116200121212     c                   if        %subst(wnora:279:2)='++'
116300121212     c                   leavesr
116400121212     c                   endif
116500121212     c                   eval      %subst(wnoRa:Pos)=
116600121213     c                             wciasm
116700121211     c                   else
116800121213     c                   eval      wnoRa=wciasm
116900121211     c                   endif
117000121211     c                   eval      wnoRA=%trim(wnora) + ' '  +
117100121212     c                             or_vrbvas + ' ' +
117200121212     c                             %trim(%editc(or_vrbias:'4')) +
117300121213     c                             ' A: ' + vrbvas + ' ' +
117400121211     c                             %editc(vrbias:'4')
117500121211     c                   endsl
117600121211     c                   endif
117700140124     c* Flag evento RIC
117800140124     c                   if        or_vrbric= ' ' and vrbric='S'
117900140124     c                   if        wnoRA<>*blanks
118000140124     c                   exsr      ver_pos
118100140124     c                   if        %subst(wnora:279:2)='++'
118200140124     c                   leavesr
118300140124     c                   endif
118400140124     c                   eval      %subst(wnoRa:Pos)=
118500140124     c                             wcric
118600140124     c                   else
118700140124     c                   eval      wnoRa=wcric
118800140124     c                   endif
118900140124     c                   endif
119000121210     c                   endsr
119100121206      *------------------------------------------------------------------------*
119200121206      * Aggiorna TIVRB come elaborato
119300121213     c*------------------------------------------------------------------------*
119400121206     c     Updvrb        BegSr
119500121218
119600121218     c* SOLO SE NON IN SIMULAZIONE
119700121218     c                   if        w_simul=*blanks
119800121218
119900121213     c* flaggo sia il record originale che il record "VA"
120000121213     c     kvrb          readpe    tivrb01l
120100121213     c                   z-add     woggi         vrbdela
120200121217     c                   eval      vrbflg=wflg
120300121213     c                   update    tivrb000
120400121213     c     kvrb          reade     tivrb01l
120500121206     c                   z-add     woggi         vrbdela
120600121217     c                   eval      vrbflg=wflg
120700121206     c                   update    tivrb000
120800121218
120900121218     c                   endif
121000121218
121100121206     c                   endsr
121200121212      *------------------------------------------------------------------------*
121300121212      * Verifica posizione per sottostringa su nota RA
121400121212      *------------------------------------------------------------------------*
121500121212     c     ver_pos       BegSr
121600121212     c                   clear                   pos
121700121212     c                   eval      pos=%checkr(' ':wnora)
121800121212     c                   if        pos>=279
121900121212     c                   eval      %subst(wnora:279:2)='++'
122000121212     c                   else
122100121212     c                   eval      risult=%div(pos:35)
122200121212     c                   eval      rem=%rem(pos:35)
122300121212     c                   if        rem>0
122400121212     c                   eval      risult+=1
122500121212     c                   endif
122600121212     c                   eval      pos=(risult*35)+1
122700121212     c                   if        pos>=279
122800121212     c                   eval      %subst(wnora:279:2)='++'
122900121212     c                   endif
123000121212     c                   endif
123100121212     c                   endsr
123200121207      *------------------------------------------------------------------------*
123300121218      * Recupera dstb
123400121207      *------------------------------------------------------------------------*
123500121207     c     rep_dstb      BegSr
123600121207     c                   Clear                   dstb
123700121207     c                   Eval      TblCod = 'TB'
123800121207     c                   Movel(p)  f_tastbl      TblKey
123900121207     c     kTab          Chain     Tabel00f
124000121207     c                   if        %found(tabel00f)
124100121207     c                   eval      dstb=tbluni
124200121207     c                   endif
124300121207     c                   endsr
124400121221      *------------------------------------------------------------------------*
124500121221      * Scrittura Work-file
124600121221      *------------------------------------------------------------------------*
124700121221     c     wrt_wf        BegSr
124800121221     c                   clear                   wfrpf000
124900121221     c* filiale del cliente
125000121221     c                   movel     tasksc        rpffil
125100121221     c                   eval      RPFFID=orgdes
125200121221     c* distretto
125300121221     c                   eval      RPFDIV=orgfl3
125400121221     c                   clear                   ds17
125500121221     c                   Eval      TblCod = '17'
125600121221     c                   Movel(p)  rpfdiv        TblKey
125700121221     c     kTab          Chain     Tabel00f
125800121221     c                   if        %found(tabel00f)
125900121221     c                   movel     tbluni        ds17
126000121221     c                   endif
126100121221     c                   eval      RPFDID=§17des
126200121221     c* area
126300121221     c                   eval      RPFARE=orgcar
126400121221     c                   clear                   ds05
126500121221     c                   Eval      TblCod = '05'
126600121221     c                   Movel(p)  rpfare        TblKey
126700121221     c     kTab          Chain     Tabel00f
126800121221     c                   if        %found(tabel00f)
126900121221     c                   movel     tbluni        ds05
127000121221     c                   endif
127100121221     c                   eval      RPFARD=§05des
127200121221
127300121221     c                   eval      RPFDAT=woggi
127400121221     c                   eval      RPFAASR=tasaas
127500121221     c                   EVAL      RPFLNPR=TASLNP
127600121221     c                   EVAL      RPFNRSR=tasnrs
127700121221     c                   eval      RPFNSPR=tasnsp
127800121221     c                   eval      RPFIMV =tasimv
127900121221     c                   eval      RPFKSC =tasksc
128000121221     c                   eval      RPFRAG=s_acorag
128100140507     c                   eval      RPFMTR=ida06no1
128200121221     c                   eval      RPFAASO=f_tasaas
128300121221     c                   eval      RPFLNPO=f_taslnp
128400121221     c                   eval      RPFNRSO=f_tasnrs
128500121221     c                   eval      RPFNSPO=f_tasnsp
128600121221     c                   eval      RPFMGSO=f_tasmgs
128700121221
128800121221     c                   if        w_simul='S'
128900121221     c                   write     wfrpfs
129000121221     c                   else
129100121221     c                   write     wfrpf000
129200121221     c                   endif
129300121221
129400121221     c                   endsr
129500060717      *------------------------------------------------------------------------*
129600060714      * Routine iniziale
129700060714      *------------------------------------------------------------------------*
129800060714     c     *Inzsr        BegSr
129900060714
130000060714     c     *Entry        Plist
130100060714     c                   Parm                    kpjba
130200121218     c                   movel     kpjbu         w_simul
130300130109     c                   eval      w_area=%subst(kpjbu:2:1)
130400060717
130500121221     c* Apertura del file di work
130600121218     c                   if        w_simul='S'
130700121221     c                   open      wfrpf00s
130800121221     c                   else
130900121221     c                   open      wfrpf00f
131000121221     c                   endif
131100060714
131200060714     c     *dtaara       define    §azute        azuteds
131300060714     c     *dtaara       define    §datiute      ddatiute
131400060714     c                   in(E)     *dtaara
131500060714     c                   If        %error  or RSUT = *blanks
131600060714     c                   Clear                   Tibs34ds
131700060714     c                   Call      'TIBS34R'
131800060714     c                   Parm                    Tibs34ds
131900060714     c                   In        *dtaara
132000060714     c                   EndIf
132100060714
132200121127      * reperisco la data corrente
132300121127     c                   time                    dataiso_cor
132400121127     c                   move      dataiso_cor   woggi
132500121127     c                   eval      gg=%subdt(dataiso_cor:*days)
132600060717      * chiavi
132700060717
132800060717      * Tntbe01l
132900060717     c     Ktbe          Klist
133000060717     c                   Kfld                    Kcode
133100060717     c                   Kfld                    Kke1
133200121127      * Titas30c
133300121127     c     kTas          klist
133400121127     c                   kfld                    vrbAas
133500121127     c                   kfld                    vrbLnp
133600121127     c                   kfld                    vrbNrs
133700121127     c                   kfld                    vrbNsp
133800121127     c                   kfld                    vrbtbl
133900121207     C     ktastrc       KLIST
134000121207     C                   KFLD                    vrbAAS
134100121207     C                   KFLD                    vrbLNP
134200121207     C                   KFLD                    vrbNRS
134300121207     C                   KFLD                    vrbNSP
134400121207     C                   KFLD                    Ktrc
134500060721      * Tabel00f
134600060721     c     Ktab          Klist
134700060721     c                   kfld                    TblKut
134800060721     c                   kfld                    TblCod
134900060721     c                   kfld                    TblKey
135000121211     c     kFiar5        Klist
135100121211     c                   Kfld                    vrbAas
135200121211     c                   Kfld                    vrbLnp
135300121211     c                   Kfld                    vrbNrs
135400121211     c                   Kfld                    vrbNsp
135500121211     c                   Kfld                    kAr5Trd
135600121211      * Chiave file eventi FNEVB
135700140127     c*    Kevb4         Klist
135800140127     c*                  Kfld                    vrbaas
135900140127     c*                  Kfld                    vrblnp
136000140127     c*                  Kfld                    vrbnrs
136100140127     c*                  Kfld                    vrbnsp
136200140127     c*                  Kfld                    Wcev
136300121211     c     ktai05        klist
136400121211     c                   kfld                    vrbaas
136500121211     c                   kfld                    vrblnp
136600121211     c                   kfld                    vrbnrs
136700121211     c                   kfld                    vrbnsp
136800121211     c                   kfld                    vrbtbl
136900121213     c     kvrb          klist
137000121213     c                   kfld                    vrbaas
137100121213     c                   kfld                    vrblnp
137200121213     c                   kfld                    vrbnrs
137300121213     c                   kfld                    vrbnsp
137400121213     c                   kfld                    vrbtbl
137500060718
137600060721     c                   Eval      TblKut = 1
137700121213
137800121213     c* Chain tabella 3a con codice bolla utilizzato per creare le bolle di recupero
137900121213     c* variazioni post-fatturazione
138000121213      * Tipo bolla
138100121213     c                   Clear                   ds3a_sav
138200121213     c                   Eval      TblCod = '3A'
138300121213     c                   Movel(p)  'BV'          TblKey
138400121213     c     kTab          Chain     Tabel00f
138500121213     c                   If        Not %Found(Tabel00f) or TblFlg <> *Blanks
138600121213     c                   eval      *inlr=*on
138700121213     c                   return
138800121213     c                   endif
138900121213     c                   Eval      ds3a_sav = TblUni
139000121206
139100121206     c                   exsr      rec_RPFarea
139200060717
139300060714     c                   EndSr
139400121227      *------------------------------------------------------------------------*
139500121227      * Routine finale
139600121227      *------------------------------------------------------------------------*
139700121227     c     routend       BegSr
139800121227
139900121227     C                   MOVEL     'C'           T02TLA
140000121227     C                   CALL      'TIBS02R'
140100121227     C                   PARM                    KPJBA
140200121227     C                   PARM                    tibs02ds
140300121227
140400121227     c                   Eval      i69tla = 'C'
140500121227     c                   Call      'TIBS69R'
140600121227     c                   Parm                    tibs69ds
140700121227
140800121227     C                   MOVEL     'C'           D55TLA
140900121227     C                   CALL      'FNLV55R'
141000121227     C                   PARM                    FNLV55DS
141100121227
141200121227     c                   Eval      *InLr = *On
141300121227
141400121227     c                   EndSr
141500121206      ****************************************************
141600121206      ** Recupero aree clienti elaborabili
141700121206      ****************************************************
141800121206     c     rec_RPFarea   Begsr
141900121206     c                   Clear                   Tibs02ds
142000121211     c                   Clear                   camposare
142100130325     c                   Clear                   w_tuttea
142200121206     c                   Eval      T02Mod = 'C'
142300121206     c                   Eval      T02Sif = knsif
142400121206     c                   Eval      T02Cod = 'RPF'
142500121206     c                   Movel(p)  'AREA'        T02Ke1
142600121206     c                   Call      'TIBS02R'
142700121206     c                   Parm                    Kpjba
142800121206     c                   Parm                    Tibs02ds
142900121206     c                   If        T02Err = *Blanks
143000121211     c                   eval      camposare=t02uni
143100121206     c                   EndIf
143200130325     c                   if        %lookup('999':sare)>0
143300130325     c                   eval      w_tuttea='S'
143400130325     c                   endif
143500121206     c                   endsr
