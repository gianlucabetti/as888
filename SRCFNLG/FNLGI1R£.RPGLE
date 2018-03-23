000100160819     H*------------------------------------------------------------------------*
000200160926     H* Driver per le  bolla la cui merce deve essere portata nel magazzino giac
000300160819     H*------------------------------------------------------------------------*
000400160926     ffnarb01l  if   e           k disk
000500170323     fazorg01l  if   e           k disk
000600160926     ftigcp02l  if   e           k disk
000700160926     ftigcp21l  if   e           k disk    rename(tigcp000:tigcp021)
000800160822     FFNevb01L  IF   E           K DISK
000900160822     FTABEL00F  IF   E           K DISK    usropn
001000161005     Ftntbe01l  IF   E           K DISK
001100160822
001200160822     D codntc          S              3    DIM(250)
001300160926     D msg             S             78    DIM(2) ctdata perrcd(1)
001400070629
001500050221     D KPJBA         E DS
001600160822     D ds2a          E DS
001700160926     D ds2g          E DS
001800170323     D og150         E DS
001900170323     D fnlv55ds      E DS
002000160822     d
002100160926     D FNLGI1DS      E DS
002200161018     D tigcp00pr     E DS                  extname(tigcp00f) prefix(pr_)
002300161018     D tigcp00ds     E DS                  extname(tigcp00f)
002400161003     d
002500161003     d TRULVPODS     e ds
002600161003     D  skFilOk               16    765    dim(250)
002700160822     d
002800160822     D WDAT8           DS
002900160822     D  DATADA                 1      8  0
003000160822     D  DATAA                  9     16  0
003100160822     D  GGL                   17     21  0
003200160822     D tfptfa          DS
003300160822     D  p_tfp                  1      3  0
003400160822     D  p_tfa                  4      6  0
003500160927     D
003600160927      *  si elaborano solo i progressivi = 0 ovvero in essere
003700160927     d kfrg            s                   like(gcpfrg) inz
003800160927     d
003900160927     d waddgg          s              3  0
004000160926     d datagiac        s              8  0
004100170323     d picking         s              1
004200161005     d ktcod           s                   like(tbecod) inz('FLG')
004300161005     d ktke1           s                   like(tbeke1)
004400160926     d Datasys         s               d   datfmt(*iso) inz(*sys)
004500070629      *----------------------------------------------------------------
004600160926     C     *ENTRY        PLIST
004700160822     C                   PARM                    KPJBA
004800161018     C                   PARM                    tigcp00pr
004900160926     C                   MOVEL     KPJBU         fnlgi1ds
005000170323
005100161018     c                   if        %parms=2
005200161018     c                   eval      tigcp00ds=tigcp00pr
005300161018     c                   else
005400161018     c                   clear                   tigcp00ds
005500161018     c                   endif
005600161018     c
005700161003     c* carico le filiali abilitate ai 2 gg in IMA
005800161003     c                   clear                   trulvpods
005900161003     c                   eval      ivpoke1 = 'DECOFIIMG'
006000161003     c                   call      'TRULVPOR'
006100161003     c                   parm                    trulvpods
006200160926
006300160927    0c                   if        ilgi1tla<>'C'
006400160926     c                   clear                   OLGI1ERR
006500160926     c                   clear                   OLGI1msg
006600161003     c                   clear                   OLGI1aMG
006700161003     c                   clear                   OLGI1inMG
006800161003     c                   clear                   OLGI1outMG
006900160926     c
007000160926     c* se non passato record di giacenza lo carico
007100160927    1c                   if        %parms=1 or gcpngc=0
007200160926     c* per numero sped
007300160926    2c                   if        ilgi1nsp>0
007400160926     c     kgcp21        chain     tigcp21l
007500160926    3c                   if        not %found(tigcp21l)
007600160926     c                   eval      olgi1err='E'
007700160926     c                   eval      olgi1msg=msg(1)
007800160927     c                   goto      fine
007900160926    3c                   endif
008000160926     c
008100160926   x2c                   else
008200160926
008300160926     c     kgcp02        chain     tigcp02l
008400160926    3c                   if        not %found(tigcp02l)
008500160926     c                   eval      olgi1err='E'
008600160926     c                   eval      olgi1msg=msg(1)
008700160927     c                   goto      fine
008800160926    3c                   endif
008900160926    2c                   endif
009000160926    1c                   endif
009100161003
009200161003     c                   movel     datasys       dateu             8 0
009300161003
009400161003     c* Se la giacenza ha la data uscita da MG --> imposto il relativo flag
009500161003    1c                   if        gcpduma>0 and gcpduma=dateu
009600161003     c                   eval      OLGI1outMG='Y'
009700161003     c                   goto      fine
009800161003    1c                   endif
009900161003    1c                   if        gcpduma>0 and gcpduma<dateu
010000161003     c                   eval      OLGI1outMG='P'
010100161003     c                   goto      fine
010200161003    1c                   endif
010300161003     c* Se la giacenza ha la data entrata a MG --> imposto il relativo flag
010400161007    1c***                if        gcpduma=0  and gcpdlma>0
010500161007     c* La logica vorrebbe che la merce esce solo se è entrata
010600161007     c*  ma visto che il pgm prima stampava anche le giacenze senza data a
010700161007     c*  mag giac. lo faccio anche qui
010800161007    1c                   if        gcpduma=0
010900161003    2c                   if        gcpfas = 35 or gcpfas>36
011000161003     c                   eval      OLGI1outMG='S'
011100161003     c                   goto      fine
011200161003    2c                   endif
011300161003    1c                   endif
011400161003
011500161003     c* Se la giacenza ha la data entrata a MG --> imposto il relativo flag
011600161003    1c                   if        gcpdlma>0 and gcpdlma=dateu
011700161003     c                   eval      OLGI1aMG='Y'
011800161003     c                   goto      fine
011900161003    1c                   endif
012000161003
012100161003    1c                   if        gcpdlma>0 and gcpdlma<dateu
012200161003     c                   eval      OLGI1inMG='S'
012300161003     c                   goto      fine
012400161003    1c                   endif
012500160822     c
012600161003     c* se non si sono verificati tutti questi casi, allora verifico
012700161003     c* se da portare a MG oppure no --> imposto il falg a "N"
012800161003     c                   eval      OLGI1AMG='N'
012900160822
013000161003      * la fase non può essere > 30
013100161003    1c                   if        gcpfas > 30
013200160927     c                   goto      fine
013300160926    1c                   end
013400161003     c
013500160926      * Se riapertura --> da portare subito a IMG
013600160926    1c                   if        GCPRIAP='S'
013700161003     c                   eval      OLGI1AMG='S'
013800160927     c                   goto      fine
013900160926    1c                   endif
014000161003
014100161003     c* Se la lin di arrivo non è in tabella attivazione --> subito in IMG
014200161003     c                   if        %lookup('999':skFilOk)>0 or
014300161003     c                             %lookup(%editc(GCPLNAA:'X'):skFilOk)>0
014400161003     c                   else
014500161003     c                   eval      OLGI1AMG='S'
014600161003     c                   goto      fine
014700161003     c                   endif
014800170323
014900170323     c* se non ha il picking --> subito a IMG
015000170323     c                   exsr      carica_pick
015100170323     c
015200170323     c                   if        picking<>'S'
015300170323     c                   eval      OLGI1AMG='S'
015400170323     c                   goto      fine
015500170323    1c                   endif
015600161003     c
015700161003     c* alcune lnp     vanno subito a img
015800161006     c* prima controllo lnp poi ccm
015900161006     c                   eval      ktke1=%editc(GCPLNPA:'X')
016000161006     c     ktbe          chain     tntbe01l
016100161006     c                   if        not %found(tntbe01l) or tbeatb<>' '
016200161003     c* alcuni clienti vanno subito a img
016300161006     c                   eval      ktke1=%editc(GCPSCM:'X')
016400161006     c     ktbe          chain     tntbe01l
016500161006     c                   endif
016600161006     c                   if        %found(tntbe01l) and tbeatb=' '
016700161006     c                   eval      OLGI1AMG='S'
016800161006     c                   goto      fine
016900161006    1c                   endif
017000160926
017100161003     c* Se il campo PESO non è stato passato chain sulla bolla
017200160926    1c                   if        ilgi1pkf=0
017300160926     c     karb          chain     fnarb01l
017400160926    2c                   if        %found(fnarb01l)
017500160926    3c                   select
017600160926     c                   when      arbpkc>0 and arbncp=arbncl
017700160926     c                   eval      ilgi1pkf=arbpkc
017800160926     c                   when      arbpkc>0 and arbncp<arbncl
017900160926     c                             and arbpkc>arbpkf
018000160926     c                   eval      ilgi1pkf=arbpkc
018100160926     c                   other
018200160926     c                   eval      ilgi1pkf=arbpkf
018300160926    3c                   endsl
018400160926    2c                   endif
018500160926    1c                   endif
018600160822
018700160926    1c                   if        ilgi1pkf>§2gkgimg
018800161003     c                   eval      OLGI1AMG='S'
018900160927     c                   goto      fine
019000160926    1c                   endif
019100160926     c
019200160927     c* Cerco l'ultimo evento di mancata consegna per vedere se aggiungere
019300160926     c*  2 o 3 giorni
019400170406     c* considero solo gli eventi da chiusura distinta
019500161026     c                   eval      waddgg=§2gg_img
019600160926     c
019700160822     c     karb          setgt     fnevb01l
019800160822     c     karb          readpe    fnevb01l
019900160927    1c                   dow       not %eof(fnevb01l)
020000160822
020100160927    2c                   if        %lookup(evbcev:codntc)>0
020200170406     c                             and %subst(evbnot:8:7)>*zeros
020300160822     c* trovato evento di mancata consegna --> verifico la data rispetto
020400160822     c*  alla data giacenza
020500160926     c                   z-add     gcpmgc        datagiac
020600160926     c                   movel     gcpagc        datagiac          8 0
020700160927    3c                   if        datagiac=evbdev
020800161026     c                   eval      waddgg=waddgg+1
020900160927    3c                   endif
021000160926     c                   leave
021100160927    2c                   endif
021200160926     c
021300160926     c     karb          readpe    fnevb01l
021400160927    1c                   enddo
021500160926
021600160926     c* Passati due o tre giorni dall'apertura giacenza
021700160926     c*                                         i colli passano in IMG
021800160824     c                   clear                   wdat8
021900160824     c                   clear                   tfptfa
022000160824     c                   z-add     gcpmgc        datada
022100160824     c                   movel     gcpagc        datada
022200160926     c                   movel     datasys       dataa
022300160926     c                   z-add     GCPLNAA       p_tfa
022400160824     c                   call      'XSRLAV8'
022500160824     c                   parm                    wdat8
022600160824     c                   parm                    tfptfa
022700160824
022800160927    1c                   if        GGL > waddgg
022900161003     c                   eval      OLGI1AMG='S'
023000160927    1c                   endif
023100160927
023200160927    0c                   endif
023300160926
023400160926     c
023500160927     c     fine          tag
023600161007     c                   movel     fnlgi1ds      kpjbu
023700160926     c                   if        ilgi1tla=' '
023800160926     c                   return
023900160926     c                   else
024000160926     c                   seton                                        lr
024100160926     c                   endif
024200170323     C*---------------------------------------------------------------*
024300170323     C     carica_pick   BEGSR
024400170323     C*---------------------------------------------------------------*
024500170323     c* se la filiale gestine è uguale al primo richiamo --> tengo
024600170323     c*  buoni i dati anche se non passato
024700170323     c                   if        (ilgi1fgs>*zeros and
024800170323     c                             wfgs<> %int(ilgi1fgs)) or
024900170323     c                             wfgs<>gcpfgc
025000170323     c* se il campo filiale gestione non è stato passato lo carico
025100170323     c                   clear                   wfgs
025200170323     c                   clear                   picking
025300170323     c
025400170323    1c                   if        ILGI1PICK=' '
025500170323     c
025600170323    2c                   if        ILGI1FGS<=*zeros
025700170323     c                   clear                   fnlv55ds
025800170323     c                   eval      d55lin=GCPFGC
025900170323     c                   eval      d55tpt='6'
026000170323     c                   movel     dateu         d55drf
026100170323     c                   call      'FNLV55R'
026200170323     c                   parm                    fnlv55ds
026300170323     c                   movel     d55tfa        wfgs              3 0
026400170323   x2c                   else
026500170323     c                   movel     ILGI1FGS      wfgs              3 0
026600170323    2c                   endif
026700170323     c
026800170323     c     wfgs          chain     azorg01l
026900170323     c                   movel     orgdf0        og150
027000170323     c                   eval      picking=§OGPCK
027100170323     c                   if        picking=' '
027200170323     c                   eval      picking='N'
027300170323     c                   endif
027400170323
027500170323   x1c                   else
027600170323     c* passato
027700170323     c                   eval      picking=ILGI1PICK
027800170323    1c                   endif
027900170323    1c                   endif
028000170323     c*
028100170323     c                   ENDSR
028200050221     C*---------------------------------------------------------------*
028300050221     C     *INZSR        BEGSR
028400050221     C*---------------------------------------------------------------*
028500160822     c
028600160822     c* Carico i codici di mancata consegna
028700160822     c                   eval      kcod='2A'
028800160822     c                   eval      codut=1
028900160822     c                   clear                   xx
029000160822     c                   open      tabel00f
029100160822     c     ktab          setll     tabel00f
029200160822     c     ktab          reade     tabel00f
029300160822     c                   dow       not %eof(tabel00f)
029400160822     c                   eval      ds2a=tbluni
029500161007     c                   if        §2antc='S'  or (§2atip<>'§'
029600161007     c                             and §2atip<>'S')
029700160822     c                   add       1             xx                3 0
029800160823     c                   movel     tblkey        codntc(xx)
029900160822     c                   endif
030000160822     c     ktab          reade     tabel00f
030100160822     c                   enddo
030200160926     c
030300160926     c* chaino DS2G per il peso limite sopra il quale va subito in IMG
030400160926     c                   eval      kcod='2G'
030500160926     c                   eval      kkey='1       '
030600160926     c     ktab2         chain     tabel00f
030700160926     c                   if        %found(tabel00f)
030800160926     c                   eval      ds2g=tbluni
030900160926     c                   else
031000160926     c                   eval      §2gkgimg=50
031100160926     c                   endif
031200160926     c
031300160926     c                   close     tabel00f
031400170323     c                   clear                   wfgs
031500170323     c                   clear                   picking
031600050221      *
031700050221     C* DEFINIZIONE CHIAVI
031800160822     C     Ktab          KLIST
031900160822     C                   KFLD                    codut             1 0
032000160822     C                   KFLD                    kcod              2
032100160926     C     Ktab2         KLIST
032200160926     C                   KFLD                    codut             1 0
032300160926     C                   KFLD                    kcod              2
032400160926     C                   KFLD                    kkey              8
032500161006     C     Ktbe          KLIST
032600161006     C                   KFLD                    ktcod
032700161006     C                   KFLD                    ktke1
032800050221     C*
032900050221     C     KARB          KLIST
033000160822     C                   KFLD                    GCPAASa
033100050412     C                   KFLD                    GCPLNPa
033200050412     C                   KFLD                    GCPNRSa
033300050412     C                   KFLD                    GCPNSPa
033400160927     c
033500160926     C     Kgcp21        KLIST
033600160926     C                   KFLD                    ilgi1aas
033700160926     C                   KFLD                    ilgi1lnp
033800160926     C                   KFLD                    ilgi1nrs
033900160926     C                   KFLD                    ilgi1nsp
034000160927     C                   KFLD                    kfrg
034100160926     C     Kgcp02        KLIST
034200160926     C                   KFLD                    ilgi1agc
034300160926     C                   KFLD                    ilgi1fgc
034400160926     C                   KFLD                    ilgi1ngc
034500160927     C                   KFLD                    kfrg
034600071026     C*
034700160822     C                   ENDSR
034800160926**
034900160926Giacenza non trovata
