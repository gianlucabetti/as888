000100160819     H*------------------------------------------------------------------------*
000200160926     H* Driver per le  bolla la cui merce deve essere portata nel magazzino giac
000300160819     H*------------------------------------------------------------------------*
000400160926     ffnarb01l  if   e           k disk
000500160926     ftigcp02l  if   e           k disk
000600160926     ftigcp21l  if   e           k disk    rename(tigcp000:tigcp021)
000700160822     FFNevb01L  IF   E           K DISK
000800160822     FTABEL00F  IF   E           K DISK    usropn
000900161005     Ftntbe01l  IF   E           K DISK
001000160822
001100160822     D codntc          S              3    DIM(250)
001200160926     D msg             S             78    DIM(2) ctdata perrcd(1)
001300070629
001400050221     D KPJBA         E DS
001500160822     D ds2a          E DS
001600160926     D ds2g          E DS
001700160822     d
001800160926     D FNLGI1DS      E DS
001900161018     D tigcp00pr     E DS                  extname(tigcp00f) prefix(pr_)
002000161018     D tigcp00ds     E DS                  extname(tigcp00f)
002100161003     d
002200161003     d TRULVPODS     e ds
002300161003     D  skFilOk               16    765    dim(250)
002400160822     d
002500160822     D WDAT8           DS
002600160822     D  DATADA                 1      8  0
002700160822     D  DATAA                  9     16  0
002800160822     D  GGL                   17     21  0
002900160822     D tfptfa          DS
003000160822     D  p_tfp                  1      3  0
003100160822     D  p_tfa                  4      6  0
003200160927     D
003300160927      *  si elaborano solo i progressivi = 0 ovvero in essere
003400160927     d kfrg            s                   like(gcpfrg) inz
003500160927     d
003600160927     d waddgg          s              3  0
003700160926     d datagiac        s              8  0
003800161005     d ktcod           s                   like(tbecod) inz('FLG')
003900161005     d ktke1           s                   like(tbeke1)
004000160926     d Datasys         s               d   datfmt(*iso) inz(*sys)
004100070629      *----------------------------------------------------------------
004200160926     C     *ENTRY        PLIST
004300160822     C                   PARM                    KPJBA
004400161018     C                   PARM                    tigcp00pr
004500160926     C                   MOVEL     KPJBU         fnlgi1ds
004600161018     c                   if        %parms=2
004700161018     c                   eval      tigcp00ds=tigcp00pr
004800161018     c                   else
004900161018     c                   clear                   tigcp00ds
005000161018     c                   endif
005100161018     c
005200161003     c* carico le filiali abilitate ai 2 gg in IMA
005300161003     c                   clear                   trulvpods
005400161003     c                   eval      ivpoke1 = 'DECOFIIMG'
005500161003     c                   call      'TRULVPOR'
005600161003     c                   parm                    trulvpods
005700160926
005800160927    0c                   if        ilgi1tla<>'C'
005900160926     c                   clear                   OLGI1ERR
006000160926     c                   clear                   OLGI1msg
006100161003     c                   clear                   OLGI1aMG
006200161003     c                   clear                   OLGI1inMG
006300161003     c                   clear                   OLGI1outMG
006400160926     c
006500160926     c* se non passato record di giacenza lo carico
006600160927    1c                   if        %parms=1 or gcpngc=0
006700160926     c* per numero sped
006800160926    2c                   if        ilgi1nsp>0
006900160926     c     kgcp21        chain     tigcp21l
007000160926    3c                   if        not %found(tigcp21l)
007100160926     c                   eval      olgi1err='E'
007200160926     c                   eval      olgi1msg=msg(1)
007300160927     c                   goto      fine
007400160926    3c                   endif
007500160926     c
007600160926   x2c                   else
007700160926
007800160926     c     kgcp02        chain     tigcp02l
007900160926    3c                   if        not %found(tigcp02l)
008000160926     c                   eval      olgi1err='E'
008100160926     c                   eval      olgi1msg=msg(1)
008200160927     c                   goto      fine
008300160926    3c                   endif
008400160926    2c                   endif
008500160926    1c                   endif
008600161003
008700161003     c                   movel     datasys       dateu             8 0
008800161003
008900161003     c* Se la giacenza ha la data uscita da MG --> imposto il relativo flag
009000161003    1c                   if        gcpduma>0 and gcpduma=dateu
009100161003     c                   eval      OLGI1outMG='Y'
009200161003     c                   goto      fine
009300161003    1c                   endif
009400161003    1c                   if        gcpduma>0 and gcpduma<dateu
009500161003     c                   eval      OLGI1outMG='P'
009600161003     c                   goto      fine
009700161003    1c                   endif
009800161003     c* Se la giacenza ha la data entrata a MG --> imposto il relativo flag
009900161007    1c***                if        gcpduma=0  and gcpdlma>0
010000161007     c* La logica vorrebbe che la merce esce solo se è entrata
010100161007     c*  ma visto che il pgm prima stampava anche le giacenze senza data a
010200161007     c*  mag giac. lo faccio anche qui
010300161007    1c                   if        gcpduma=0
010400161003    2c                   if        gcpfas = 35 or gcpfas>36
010500161003     c                   eval      OLGI1outMG='S'
010600161003     c                   goto      fine
010700161003    2c                   endif
010800161003    1c                   endif
010900161003
011000161003     c* Se la giacenza ha la data entrata a MG --> imposto il relativo flag
011100161003    1c                   if        gcpdlma>0 and gcpdlma=dateu
011200161003     c                   eval      OLGI1aMG='Y'
011300161003     c                   goto      fine
011400161003    1c                   endif
011500161003
011600161003    1c                   if        gcpdlma>0 and gcpdlma<dateu
011700161003     c                   eval      OLGI1inMG='S'
011800161003     c                   goto      fine
011900161003    1c                   endif
012000160822     c
012100161003     c* se non si sono verificati tutti questi casi, allora verifico
012200161003     c* se da portare a MG oppure no --> imposto il falg a "N"
012300161003     c                   eval      OLGI1AMG='N'
012400160822
012500161003      * la fase non può essere > 30
012600161003    1c                   if        gcpfas > 30
012700160927     c                   goto      fine
012800160926    1c                   end
012900161003     c
013000160926      * Se riapertura --> da portare subito a IMG
013100160926    1c                   if        GCPRIAP='S'
013200161003     c                   eval      OLGI1AMG='S'
013300160927     c                   goto      fine
013400160926    1c                   endif
013500161003
013600161003     c* Se la lin di arrivo non è in tabella attivazione --> subito in IMG
013700161003     c                   if        %lookup('999':skFilOk)>0 or
013800161003     c                             %lookup(%editc(GCPLNAA:'X'):skFilOk)>0
013900161003     c                   else
014000161003     c                   eval      OLGI1AMG='S'
014100161003     c                   goto      fine
014200161003     c                   endif
014300161003     c
014400161003     c* alcune lnp     vanno subito a img
014500161006     c* prima controllo lnp poi ccm
014600161006     c                   eval      ktke1=%editc(GCPLNPA:'X')
014700161006     c     ktbe          chain     tntbe01l
014800161006     c                   if        not %found(tntbe01l) or tbeatb<>' '
014900161003     c* alcuni clienti vanno subito a img
015000161006     c                   eval      ktke1=%editc(GCPSCM:'X')
015100161006     c     ktbe          chain     tntbe01l
015200161006     c                   endif
015300161006     c                   if        %found(tntbe01l) and tbeatb=' '
015400161006     c                   eval      OLGI1AMG='S'
015500161006     c                   goto      fine
015600161006    1c                   endif
015700160926
015800161003     c* Se il campo PESO non è stato passato chain sulla bolla
015900160926    1c                   if        ilgi1pkf=0
016000160926     c     karb          chain     fnarb01l
016100160926    2c                   if        %found(fnarb01l)
016200160926    3c                   select
016300160926     c                   when      arbpkc>0 and arbncp=arbncl
016400160926     c                   eval      ilgi1pkf=arbpkc
016500160926     c                   when      arbpkc>0 and arbncp<arbncl
016600160926     c                             and arbpkc>arbpkf
016700160926     c                   eval      ilgi1pkf=arbpkc
016800160926     c                   other
016900160926     c                   eval      ilgi1pkf=arbpkf
017000160926    3c                   endsl
017100160926    2c                   endif
017200160926    1c                   endif
017300160822
017400160926    1c                   if        ilgi1pkf>§2gkgimg
017500161003     c                   eval      OLGI1AMG='S'
017600160927     c                   goto      fine
017700160926    1c                   endif
017800160926     c
017900160927     c* Cerco l'ultimo evento di mancata consegna per vedere se aggiungere
018000160926     c*  2 o 3 giorni
018100161026     c                   eval      waddgg=§2gg_img
018200160926     c
018300160822     c     karb          setgt     fnevb01l
018400160822     c     karb          readpe    fnevb01l
018500160927    1c                   dow       not %eof(fnevb01l)
018600160822
018700160927    2c                   if        %lookup(evbcev:codntc)>0
018800160822     c* trovato evento di mancata consegna --> verifico la data rispetto
018900160822     c*  alla data giacenza
019000160926     c                   z-add     gcpmgc        datagiac
019100160926     c                   movel     gcpagc        datagiac          8 0
019200160927    3c                   if        datagiac=evbdev
019300161026     c                   eval      waddgg=waddgg+1
019400160927    3c                   endif
019500160926     c                   leave
019600160927    2c                   endif
019700160926     c
019800160926     c     karb          readpe    fnevb01l
019900160927    1c                   enddo
020000160926
020100160926     c* Passati due o tre giorni dall'apertura giacenza
020200160926     c*                                         i colli passano in IMG
020300160824     c                   clear                   wdat8
020400160824     c                   clear                   tfptfa
020500160824     c                   z-add     gcpmgc        datada
020600160824     c                   movel     gcpagc        datada
020700160926     c                   movel     datasys       dataa
020800160926     c                   z-add     GCPLNAA       p_tfa
020900160824     c                   call      'XSRLAV8'
021000160824     c                   parm                    wdat8
021100160824     c                   parm                    tfptfa
021200160824
021300160927    1c                   if        GGL > waddgg
021400161003     c                   eval      OLGI1AMG='S'
021500160927    1c                   endif
021600160927
021700160927    0c                   endif
021800160926
021900160926     c
022000160927     c     fine          tag
022100161007     c                   movel     fnlgi1ds      kpjbu
022200160926     c                   if        ilgi1tla=' '
022300160926     c                   return
022400160926     c                   else
022500160926     c                   seton                                        lr
022600160926     c                   endif
022700050221     C*---------------------------------------------------------------*
022800050221     C     *INZSR        BEGSR
022900050221     C*---------------------------------------------------------------*
023000160822     c
023100160822     c* Carico i codici di mancata consegna
023200160822     c                   eval      kcod='2A'
023300160822     c                   eval      codut=1
023400160822     c                   clear                   xx
023500160822     c                   open      tabel00f
023600160822     c     ktab          setll     tabel00f
023700160822     c     ktab          reade     tabel00f
023800160822     c                   dow       not %eof(tabel00f)
023900160822     c                   eval      ds2a=tbluni
024000161007     c                   if        §2antc='S'  or (§2atip<>'§'
024100161007     c                             and §2atip<>'S')
024200160822     c                   add       1             xx                3 0
024300160823     c                   movel     tblkey        codntc(xx)
024400160822     c                   endif
024500160822     c     ktab          reade     tabel00f
024600160822     c                   enddo
024700160926     c
024800160926     c* chaino DS2G per il peso limite sopra il quale va subito in IMG
024900160926     c                   eval      kcod='2G'
025000160926     c                   eval      kkey='1       '
025100160926     c     ktab2         chain     tabel00f
025200160926     c                   if        %found(tabel00f)
025300160926     c                   eval      ds2g=tbluni
025400160926     c                   else
025500160926     c                   eval      §2gkgimg=50
025600160926     c                   endif
025700160926     c
025800160926     c                   close     tabel00f
025900050221      *
026000050221     C* DEFINIZIONE CHIAVI
026100160822     C     Ktab          KLIST
026200160822     C                   KFLD                    codut             1 0
026300160822     C                   KFLD                    kcod              2
026400160926     C     Ktab2         KLIST
026500160926     C                   KFLD                    codut             1 0
026600160926     C                   KFLD                    kcod              2
026700160926     C                   KFLD                    kkey              8
026800161006     C     Ktbe          KLIST
026900161006     C                   KFLD                    ktcod
027000161006     C                   KFLD                    ktke1
027100050221     C*
027200050221     C     KARB          KLIST
027300160822     C                   KFLD                    GCPAASa
027400050412     C                   KFLD                    GCPLNPa
027500050412     C                   KFLD                    GCPNRSa
027600050412     C                   KFLD                    GCPNSPa
027700160927     c
027800160926     C     Kgcp21        KLIST
027900160926     C                   KFLD                    ilgi1aas
028000160926     C                   KFLD                    ilgi1lnp
028100160926     C                   KFLD                    ilgi1nrs
028200160926     C                   KFLD                    ilgi1nsp
028300160927     C                   KFLD                    kfrg
028400160926     C     Kgcp02        KLIST
028500160926     C                   KFLD                    ilgi1agc
028600160926     C                   KFLD                    ilgi1fgc
028700160926     C                   KFLD                    ilgi1ngc
028800160927     C                   KFLD                    kfrg
028900071026     C*
029000160822     C                   ENDSR
029100160926**
029200160926Giacenza non trovata
