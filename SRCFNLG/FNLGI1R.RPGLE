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
001700170424     D ds2g2         E DS
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
014000170424     c
014100170424     c* Se "OGGI" è nel periodo di sospensione attesa ima
014200170424     c*  --> subito a IMG
014300170424     c                   if        §2G2DIMAf>0  and dateu>=§2G2DIMAI
014400170424     c                             and dateu <=§2G2DIMAf
014500170424     c                   eval      OLGI1AMG='S'
014600170424     c                   goto      fine
014700170424    1c                   endif
014800161003
014900161003     c* Se la lin di arrivo non è in tabella attivazione --> subito in IMG
015000161003     c                   if        %lookup('999':skFilOk)>0 or
015100161003     c                             %lookup(%editc(GCPLNAA:'X'):skFilOk)>0
015200161003     c                   else
015300161003     c                   eval      OLGI1AMG='S'
015400161003     c                   goto      fine
015500161003     c                   endif
015600170323
015700170323     c* se non ha il picking --> subito a IMG
015800170323     c                   exsr      carica_pick
015900170323     c
016000170323     c                   if        picking<>'S'
016100170323     c                   eval      OLGI1AMG='S'
016200170323     c                   goto      fine
016300170323    1c                   endif
016400161003     c
016500161003     c* alcune lnp     vanno subito a img
016600161006     c* prima controllo lnp poi ccm
016700161006     c                   eval      ktke1=%editc(GCPLNPA:'X')
016800161006     c     ktbe          chain     tntbe01l
016900161006     c                   if        not %found(tntbe01l) or tbeatb<>' '
017000161003     c* alcuni clienti vanno subito a img
017100161006     c                   eval      ktke1=%editc(GCPSCM:'X')
017200161006     c     ktbe          chain     tntbe01l
017300161006     c                   endif
017400161006     c                   if        %found(tntbe01l) and tbeatb=' '
017500161006     c                   eval      OLGI1AMG='S'
017600161006     c                   goto      fine
017700161006    1c                   endif
017800160926
017900161003     c* Se il campo PESO non è stato passato chain sulla bolla
018000160926    1c                   if        ilgi1pkf=0
018100160926     c     karb          chain     fnarb01l
018200160926    2c                   if        %found(fnarb01l)
018300160926    3c                   select
018400160926     c                   when      arbpkc>0 and arbncp=arbncl
018500160926     c                   eval      ilgi1pkf=arbpkc
018600160926     c                   when      arbpkc>0 and arbncp<arbncl
018700160926     c                             and arbpkc>arbpkf
018800160926     c                   eval      ilgi1pkf=arbpkc
018900160926     c                   other
019000160926     c                   eval      ilgi1pkf=arbpkf
019100160926    3c                   endsl
019200160926    2c                   endif
019300160926    1c                   endif
019400160822
019500170424    1c                   if        ilgi1pkf>§2g2kgimg
019600161003     c                   eval      OLGI1AMG='S'
019700160927     c                   goto      fine
019800160926    1c                   endif
019900160926     c
020000160927     c* Cerco l'ultimo evento di mancata consegna per vedere se aggiungere
020100160926     c*  2 o 3 giorni
020200170406     c* considero solo gli eventi da chiusura distinta
020300170424     c                   eval      waddgg=§2g2g_img
020400160926     c
020500160822     c     karb          setgt     fnevb01l
020600160822     c     karb          readpe    fnevb01l
020700160927    1c                   dow       not %eof(fnevb01l)
020800160822
020900160927    2c                   if        %lookup(evbcev:codntc)>0
021000170406     c                             and %subst(evbnot:8:7)>*zeros
021100160822     c* trovato evento di mancata consegna --> verifico la data rispetto
021200160822     c*  alla data giacenza
021300160926     c                   z-add     gcpmgc        datagiac
021400160926     c                   movel     gcpagc        datagiac          8 0
021500160927    3c                   if        datagiac=evbdev
021600161026     c                   eval      waddgg=waddgg+1
021700160927    3c                   endif
021800160926     c                   leave
021900160927    2c                   endif
022000160926     c
022100160926     c     karb          readpe    fnevb01l
022200160927    1c                   enddo
022300160926
022400160926     c* Passati due o tre giorni dall'apertura giacenza
022500160926     c*                                         i colli passano in IMG
022600160824     c                   clear                   wdat8
022700160824     c                   clear                   tfptfa
022800160824     c                   z-add     gcpmgc        datada
022900160824     c                   movel     gcpagc        datada
023000160926     c                   movel     datasys       dataa
023100160926     c                   z-add     GCPLNAA       p_tfa
023200160824     c                   call      'XSRLAV8'
023300160824     c                   parm                    wdat8
023400160824     c                   parm                    tfptfa
023500160824
023600160927    1c                   if        GGL > waddgg
023700161003     c                   eval      OLGI1AMG='S'
023800160927    1c                   endif
023900160927
024000160927    0c                   endif
024100160926
024200160926     c
024300160927     c     fine          tag
024400161007     c                   movel     fnlgi1ds      kpjbu
024500160926     c                   if        ilgi1tla=' '
024600160926     c                   return
024700160926     c                   else
024800160926     c                   seton                                        lr
024900160926     c                   endif
025000170323     C*---------------------------------------------------------------*
025100170323     C     carica_pick   BEGSR
025200170323     C*---------------------------------------------------------------*
025300170323     c* se la filiale gestine è uguale al primo richiamo --> tengo
025400170323     c*  buoni i dati anche se non passato
025500170323     c                   if        (ilgi1fgs>*zeros and
025600170323     c                             wfgs<> %int(ilgi1fgs)) or
025700170323     c                             wfgs<>gcpfgc
025800170323     c* se il campo filiale gestione non è stato passato lo carico
025900170323     c                   clear                   wfgs
026000170323     c                   clear                   picking
026100170323     c
026200170323    1c                   if        ILGI1PICK=' '
026300170323     c
026400170323    2c                   if        ILGI1FGS<=*zeros
026500170323     c                   clear                   fnlv55ds
026600170323     c                   eval      d55lin=GCPFGC
026700170323     c                   eval      d55tpt='6'
026800170323     c                   movel     dateu         d55drf
026900170323     c                   call      'FNLV55R'
027000170323     c                   parm                    fnlv55ds
027100170323     c                   movel     d55tfa        wfgs              3 0
027200170323   x2c                   else
027300170323     c                   movel     ILGI1FGS      wfgs              3 0
027400170323    2c                   endif
027500170323     c
027600170323     c     wfgs          chain     azorg01l
027700170323     c                   movel     orgdf0        og150
027800170323     c                   eval      picking=§OGPCK
027900170323     c                   if        picking=' '
028000170323     c                   eval      picking='N'
028100170323     c                   endif
028200170323
028300170323   x1c                   else
028400170323     c* passato
028500170323     c                   eval      picking=ILGI1PICK
028600170323    1c                   endif
028700170323    1c                   endif
028800170323     c*
028900170323     c                   ENDSR
029000050221     C*---------------------------------------------------------------*
029100050221     C     *INZSR        BEGSR
029200050221     C*---------------------------------------------------------------*
029300160822     c
029400160822     c* Carico i codici di mancata consegna
029500160822     c                   eval      kcod='2A'
029600160822     c                   eval      codut=1
029700160822     c                   clear                   xx
029800160822     c                   open      tabel00f
029900160822     c     ktab          setll     tabel00f
030000160822     c     ktab          reade     tabel00f
030100160822     c                   dow       not %eof(tabel00f)
030200160822     c                   eval      ds2a=tbluni
030300161007     c                   if        §2antc='S'  or (§2atip<>'§'
030400161007     c                             and §2atip<>'S')
030500160822     c                   add       1             xx                3 0
030600160823     c                   movel     tblkey        codntc(xx)
030700160822     c                   endif
030800160822     c     ktab          reade     tabel00f
030900160822     c                   enddo
031000160926     c
031100160926     c* chaino DS2G per il peso limite sopra il quale va subito in IMG
031200170424     c                   clear                   ds2g2
031300160926     c                   eval      kcod='2G'
031400170424     c                   eval      kkey='2       '
031500160926     c     ktab2         chain     tabel00f
031600160926     c                   if        %found(tabel00f)
031700170424     c                   eval      ds2g2=tbluni
031800160926     c                   else
031900170424     c                   eval      §2g2kgimg=50
032000160926     c                   endif
032100160926     c
032200160926     c                   close     tabel00f
032300170323     c                   clear                   wfgs
032400170323     c                   clear                   picking
032500050221      *
032600050221     C* DEFINIZIONE CHIAVI
032700160822     C     Ktab          KLIST
032800160822     C                   KFLD                    codut             1 0
032900160822     C                   KFLD                    kcod              2
033000160926     C     Ktab2         KLIST
033100160926     C                   KFLD                    codut             1 0
033200160926     C                   KFLD                    kcod              2
033300160926     C                   KFLD                    kkey              8
033400161006     C     Ktbe          KLIST
033500161006     C                   KFLD                    ktcod
033600161006     C                   KFLD                    ktke1
033700050221     C*
033800050221     C     KARB          KLIST
033900160822     C                   KFLD                    GCPAASa
034000050412     C                   KFLD                    GCPLNPa
034100050412     C                   KFLD                    GCPNRSa
034200050412     C                   KFLD                    GCPNSPa
034300160927     c
034400160926     C     Kgcp21        KLIST
034500160926     C                   KFLD                    ilgi1aas
034600160926     C                   KFLD                    ilgi1lnp
034700160926     C                   KFLD                    ilgi1nrs
034800160926     C                   KFLD                    ilgi1nsp
034900160927     C                   KFLD                    kfrg
035000160926     C     Kgcp02        KLIST
035100160926     C                   KFLD                    ilgi1agc
035200160926     C                   KFLD                    ilgi1fgc
035300160926     C                   KFLD                    ilgi1ngc
035400160927     C                   KFLD                    kfrg
035500071026     C*
035600160822     C                   ENDSR
035700160926**
035800160926Giacenza non trovata
