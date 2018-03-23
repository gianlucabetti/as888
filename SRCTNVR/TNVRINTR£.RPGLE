000100970910     H*------------------------------------------------------------------------*
000200970910     H DECEDIT('0,') DATEDIT(*DMY.)
000300970910     F*------------------------------------------------------------------------*
000400970910     F* DATA BASE
000500970910     F*------------------------------------------------------------------------*
000600141204     Ftnvram2d  CF   E             WORKSTN usropn
000700141201     Ffnblp25l  if   E           k disk
000800141201     F                                     INFDS(FNblp1)
000900141201     F                                     RENAME(FNblp000:FNblp025)
001000141201     FFNblp00F  UF   E             DISK
001001141201     Ffnarb01l  uf   E           k disk    usropn
001002141201     Ftitas30c  uf   E           k disk    extfile(wlibsede) usropn
001100141201     Ftabel00f  if   E           k disk
001101141202     Ffnlbl01l  if   E           k disk
001102141202     fqsysprt   o    F  132        printer
001200060421     d
001300141128     D MSG             S             78    DIM(6) CTDATA PERRCD(1)
001400141201     d lna             s              3  0 dim(15)
001500970910     D*------------
001600141201     D fnblp1          DS
001700141201     D  blpNRR               397    400B 0
001800060421     d
001900970910     D KPJBA         E DS
002000060421     d Azuteds       e ds                  extname(Azute00f)
002100060421     d dDatiute      e ds
002200141201     d ds3a          e ds
002300060421     D TIBS34DS      E DS
002301141204     D xgiolavds     E DS
002400110504     d Dataiso         s               d   datfmt(*iso)
002500110504     d Dataeur         s               d   datfmt(*eur)
002600110504     d amgdri          s              8  0
002700141201     d amgprima        s              8  0
002800141126     d wdat            s              8  0
002801141202     d wdata           s              8  0
002900141126     d amgdat          s              8  0
003000141126     d dateu           s              8  0
003001141202     d contagg         s              5  0 inz
003002141202     d contlckblp      s              5  0 inz
003003141202     d contlckarb      s              5  0 inz
003004141202     d contlcktas      s              5  0 inz
003005141204     d pschedul        s              1
003100141126     d Datasys         s               d   datfmt(*iso)  inz(*sys)
003101141201     D Wlibsede        S             21
003102141201     D WlibsedeP       S             21    inz('GAITRAGRPS/TITAS30C')
003103141201     D WlibsedeB       S             21    INZ('GAITRAGRU /TITAS30C')
003200060421     d
003201141202     D titolo          C                   CONST('Aggiornamento Data -
003202141202     D                                     Consegna Richiesta bolle cliente -
003203141202     d                                     AMAZON "Prima DEL')
003204141201     ITITAS000      02
003205141201     ITITAS010      03
003206141201     ITITASP00      04
003300970910     C*------------------------------------------------------------------------*
003400970910     C*
003500060420     c* Impostazioni iniziali
003600141126     c                   eval      dataiso=datasys
003700141126     c                   eval      dataeur=dataiso
003800141126     c                   eval      dateu  =%dec(dataiso)
003900141126     c                   eval      v1cdri =%dec(dataeur)
003901141202     c                   eval      wdata  =%dec(dataeur)
004000141201     c                   eval      vlna1=005
004100141201     c                   eval      vlna2=124
004200141201     c                   eval      vlna3=130
004300141201     c                   eval      vlna4=123
004400141201     c                   eval      vlna5=103
004500141201     c                   eval      vlna6=049
004600141201     c                   eval      vlna7=072
004700141201     c                   eval      vlna8=067
004800141201     c                   eval      vlna9=028
004900141201     c                   eval      vlna10=001
005000141201     c                   eval      vCLI  =1662139
005002141204     c* se pgm non schedulato propongo a video data "Prima Del"=data odierna
005003141204     c* + 3gg lavorativi  ed emetto il video
005004141204    1c                   if        pschedul=' '
005005141204     c                   clear                   xgiolavds
005006141204     c                   eval      ixgldata=dateu
005007141204     c                   eval      ixglfil =vlna1
005008141204     c                   eval      ixglpa  ='A'
005009141204     c                   eval      ixgladd ='S'
005010141204     c                   eval      ixglgg  =3
005011141204     c                   eval      ixgllav ='S'
005012141204     c                   call      'XGIOLAV'
005013141204     c                   parm                    xgiolavds
005014141204     c                   Move      oxgldata      amgprima
005015141204     c                   eval      dataeur=%date(amgprima:*iso)
005016141204     c                   eval      vprima=%dec(dataeur)
005100060421     C* EMETTE IL VIDEO
005200060421     c     for01         tag
005300141201     C                   EXFMT     vram2d
005400060421     c                   setoff                                       2890
005500110504     c                   setoff                                       41
005600060421     c
005700060421     c* F3 - Fine
005800060421     c   kc              goto      fine
005900970910     C*
006000060421     c* controlli
006100060421     c                   EXSR      CTRD01
006200060421     c  nkf
006300060421     cor 90              goto      for01
006400060421     c
006500060421     c* F6 - Elaborazione
006600060421     c                   if        *inkf
006700141201     c                   exsr      elabora
006800141126     c                   endif
006801141204     c* pgm schedulato
006802141204   x1c                   else
006803141204     c                   exsr      elabora
006804141204     c                   eval      dataeur=%date(amgprima:*iso)
006805141204     c                   eval      vprima=%dec(dataeur)
006806141204    1c                   endif
006900970910     C* FINE PGM
006902141202     c                   except    p_tot
007000060421     c     fine          tag
007100970910     C                   SETON                                        LR
007200970910     C*------------------------------------------------------------------------*
007300060421     C* CONTROLLA CAMPI E DECODIFICA
007400970910     C*------------------------------------------------------------------------*
007500060421     C     CTRD01        BEGSR
007600110504     c                   clear                   amgdat
007700110504     c                   clear                   amgdri
007800110504     c
007900110504     c                   eval      wdat=v1cdri
008000110504     c                   exsr      CTRDEV
008100110504     c                   eval      v1cdri=wdat
008200110504
008300110504     c                   if        *in90
008400141126     c                   seton                                        4128
008500110504     C                   MOVEL     MSG(1)        V1cMSG
008600110504     c                   goto      endct1
008700110504    1c                   endif
008800110504     c                   eval      amgdri=amgdat
008900141126     c* no minore di oggi
009000141201     c**                 if        amgdri< dateu
009100141201     c**                 seton                                        412890
009200141201     C**                 MOVEL     MSG(5)        V1cMSG
009300141201     c**                 goto      endct1
009400141201    1c**                 endif
009500141126     c
009600141201     c* PRIMA DEL
009700141201     c                   eval      wdat=vPRIMA
009800141201     c                   exsr      CTRDEV
009900141201     c                   eval      vPRIMA=wdat
010000141201
010100141201     c                   if        *in90
010200141201     c                   seton                                        4228
010300141201     C                   MOVEL     MSG(1)        V1cMSG
010400141201     c                   goto      endct1
010500141201    1c                   endif
010600141201     c                   eval      amgprima=amgdat
010700141201     C
010800141201     C                   IF        AMGPRIMA<=amgdri
010900141201     c                   seton                                        4228
011000141201     C                   MOVEL     MSG(2)        V1cMSG
011100141201     c                   goto      endct1
011200141201    1c                   endif
011300970910     C*
011400060421     C     endct1        ENDSR
011500110504      *-----------------------------------------------------***********
011600110504      * CONTROLLo Data
011700110504      *-----------------------------------------------------***********
011800110504     c     CTRDEV        BEGSR
011900110504    1c                   if        wdat>0
012000110504     c                   movel     wdat          w002a             2
012100110504     c     *eur          test(d)                 wdat                   30
012200110504    2c                   if        *in30  and w002a='00'
012300110504     c     *dmy          test(d)                 wdat                   30
012400110504     c                   endif
012500110504     c* Errore
012600110504    3c                   if        *in30
012700110504     c                   seton                                        9028
012800110504     c                   goto      endctdev
012900110504     c                   endif
013000110504     c
013100110504     c                   if        w002a='00'
013200110504     c     *dmy          move      wdat          dataeur
013300110504     c                   else
013400110504     c     *eur          move      wdat          dataeur
013500110504     c                   endif
013600110504     c
013700110504     c                   else
013800110504     c                   seton                                        9028
013900110504     c                   goto      endctdev
014000110504     c                   endif
014100110504     c* la trasformo in AAAAAMMGG - iso
014200110504     c                   move      dataeur       wdat
014300110504     c                   movel     dataeur       dataiso
014400110504     c     *iso          movel     dataiso       amgdat            8 0
014500110504     c
014600110504     c
014700110504     c     endctdev      ENDSR
014800970910     C*------------------------------------------------------------------------*
014900970910     C* *INZSR - OPERAZIONI INIZIALI
015000970910     C*------------------------------------------------------------------------*
015100970910     C     *INZSR        BEGSR
015200970910     C*---
015300970910     C* RICEVIMENTO PARAMETRI
015400970910     C*---
015500970910     C     *ENTRY        PLIST
015600970612     C                   PARM                    KPJBA
015601141204     c                   movel     kpjbu         pschedul
015602141204     c*
015603141204     c                   if        pschedul=*blanks
015604141204     c                   open      tnvram2d
015605141204     c                   endif
015700060421     c
015800110504     c                   z-add     1             codut             1 0
015900060421      *
016000060421     c     *dtaara       define    §azute        azuteds
016100060421     c     *dtaara       define    §datiute      ddatiute
016200060421     c                   in(E)     *dtaara
016300060421    1c                   If        %error  or RSUT = *blanks
016400060421     c                   Clear                   Tibs34ds
016500060421     c                   Call      'TIBS34R'
016600060421     c                   Parm                    Tibs34ds
016700060421     c                   In        *dtaara
016800060421    1c                   EndIf
016900060421    cc                   movel     rsut          v1crsu
017000060421     c                   ENDSR
017100141201     C*------------------------------------------------------------------------*
017200141201     C     ELABORA       BEGSR
017201141202     c                   setoff                                       51
017202141201     c                   if        %subst(knmus:1:3)='EDP'
017203141202     c                   seton                                        51
017204141201     c                   open      fnarb01l
017205141201      /free
017206141201       if %subst(knsif:7:1) = 'P';
017207141201         Wlibsede = Wlibsedep;
017208141201         Else ;
017209141201         wlibsede = Wlibsedeb;
017210141201       endif;
017211141201       Open   Titas30c;
017214141201      /end-free
017215141201     c                   endif
017300141201     c                   z-add     1             XX                3 0
017400141201     c                   z-add     vlna1         lna(1)
017500141201     c                   z-add     vlna2         lna(2)
017600141201     c                   z-add     vlna3         lna(3)
017700141201     c                   z-add     vlna4         lna(4)
017800141201     c                   z-add     vlna5         lna(5)
017900141201     c                   z-add     vlna6         lna(6)
018000141201     c                   z-add     vlna7         lna(7)
018100141201     c                   z-add     vlna8         lna(8)
018200141201     c                   z-add     vlna9         lna(9)
018300141201     c                   z-add     vlna10        lna(10)
018400141201     c                   z-add     vlna11        lna(11)
018500141201     c                   z-add     vlna12        lna(12)
018600141201     c                   z-add     vlna13        lna(13)
018700141201     c                   z-add     vlna14        lna(14)
018800141201     c                   z-add     vlna15        lna(15)
018900141201     c
019000141201     c                   dow       lna(xx)>0
019100141201     c                   eval      klna=lna(xx)
019101141204     c                   if        pschedul='S'
019102141204     c                   clear                   xgiolavds
019103141204     c                   eval      ixgldata=dateu
019104141204     c                   eval      ixglfil =klna
019105141204     c                   eval      ixglpa  ='A'
019106141204     c                   eval      ixgladd ='S'
019107141204     c                   eval      ixglgg  =3
019108141204     c                   eval      ixgllav ='S'
019109141204     c                   call      'XGIOLAV'
019110141204     c                   parm                    xgiolavds
019111141204     c                   Move      oxgldata      amgprima
019112141204     c                   eval      amgdri=dateu
019113141204     c                   endif
019200141201     c                   movel     amgdri        kaas
019300141201     c                   move      amgdri        kmgs
019301141204     c* se pgm schedulato calcolo la data "prima del" da impostare
019302141204     c* (data del giorno + 3gg lavorativi)
019315141204     c
019316141204     c
019400141201     c     kblp          klist
019500141201     c                   kfld                    klna              3 0
019600141201     c                   kfld                    kdcm              8 0
019700141201     c                   kfld                    kaas              4 0
019800141201     c                   kfld                    kmgs              4 0
019900141201     c     ktab          klist
020000141201     c                   kfld                    codut             1 0
020100141201     c                   kfld                    kcod              2
020200141201     c                   kfld                    kkey              8
020300141201     c                   clear                   kdcm
020301141201     c     kbolla        klist
020302141201     c                   kfld                    blpaas
020303141201     c                   kfld                    blplnp
020304141201     c                   kfld                    blpnrs
020305141201     c                   kfld                    blpnsp
020400141201     c
020500141201     C* LEGGO LE BOLLE PARTENZA DEL CLIENTE
020600141201     c     kblp          setll     fnblp25l
020700141201     c     kblp          reade     fnblp25l
020800141201     c
020900141201     c                   dow       not %eof(fnblp25l)
021000141201     c                   select
021100141201     c                   when      blpccm>0  and blpccm<>vcli
021101141201     c     kblp          reade     fnblp25l
021200141201     c                   iter
021300141201     c
021400141201     c                   when      blpccm=0 and blpksc<>vcli
021401141201     c     kblp          reade     fnblp25l
021500141201     c                   iter
021600141201     c                   endsl
021601141202
021602141202     c* scarto se bolla figlia
021603141202     c     kbolla        setll     fnlbl01l
021604141202     c                   if        %equal(fnlbl01l)
021605141202     c     kblp          reade     fnblp25l
021606141202     c                   iter
021607141202     c                   endif
021700141201     c
021800141201     c* escludo bolle recupero
021900141201     c                   z-add     1             codut
022000141201     c                   clear                   ds3a
022100141201     c                   eval      kcod='3A'
022200141201     c                   eval      kkey=blpcbo
022300141201     c     ktab          chain     tabel
022400141201     c                   if        %found(tabel00f )
022500141201     c                   movel     tbluni        ds3a
022600141201     c* escluse le recupero
022700141201     c                   endif
022800141201     c
022900141201     c                   if        §3arbl<>'R'
023000141201     c* se non è EDP solo bolle da partire
023100141201     c                   if        %subst(knmus:1:3)<>'EDP'
023200141201     c* se già partita non aggiorno
023300141201     c                   if        blpnfv>0 and blpdbr>0 and blpft1<> ' '
023301141201     c     kblp          reade     fnblp25l
023400141201     c                   iter
023500141201     c                   endif
023600141201     c                   endif
023700141201     c
023800141201     c* aggiorno
023900141201     c                   exsr      Aggiorna
024000141201     c                   endif
024100141201     c
024200141201     c     kblp          reade     fnblp25l
024300141201     c                   enddo
024400141201     c
024500141201     c                   add       1             xx                3 0
024600141201     c                   enddo
024700141201     c
024800141201     c                   ENDSR
024900141201     O*------------------------------------------------------------------------*
025000141201     C     AGGIORNA      BEGSR
025100141201     C     BLPNRR        CHAIN(e)  FNBLP00f
025200141201    1c                   if        %error
025201141202     c                   eval      contlckblp+=1
025300141201   x1c                   else
025400141201    2c                   if        blpdcr=0
025500141201     c                   eval      blpdcr=amgprima
025600141201     c                   eval      blptcr='P'
025700141201     c                   update    fnblp000
025701141202     c                   eval      contagg+=1
025702141201    2c                   endif
025800141201     c* se sono edp e bolla in arb aggiornare anche arb e tas
025801141201    2c                   if        %subst(knmus:1:3)='EDP'
025802141201    3c                   if        blpnfv>0 and blpdbr>0 and blpft1<> ' '
025803141201     c     kbolla        chain(e)  fnarb01l
025804141201    4c                   if        %error
025805141202     c                   eval      contlckarb+=1
025806141201   x4c                   else
025807141201    5c                   if        arbdcr=0
025808141201     c                   eval      arbdcr=amgprima
025809141201     c                   eval      arbtcr='P'
025810141201     c                   update    fnarb000
025811141210
025812141210     C                   SETOFF                                       020304
025813141210     c     kbolla        setgt     titas30c
025814141210     c     kbolla        readpe(e) titas30c
025815141210    4c                   if        %error
025816141210     c                   eval      contlcktas+=1
025817141210   x4c                   else
025818141210    5c                   if        tasdcr=0
025819141210     c                   eval      tasdcr=amgprima
025820141210     c                   eval      tastcr='P'
025821141210     c   02              update    titas000
025822141210     c   03              update    titas010
025823141210     c   04              update    titasp00
025824141210    5c                   endif
025825141210    4c                   endif
025826141201    5c                   endif
025827141201    4c                   endif
025828141201    3c                   endif
025829141201    2c                   endif
025900141201    1c                   endif
026000141201     c
026100141201     c                   ENDSR
026101141202     Oqsysprt   e            P_tot             2
026102141202     O                       RSUT                20
026103141202     o                       titolo           +   5
026104141202     o                       vprima           +   1 '  /  /    '
026105141202     o                                        +   1 '"'
026106141202     O                       wdata              132 '  /  /    '
026107141202     o          e            p_tot       2
026108141202     o                                              'Bolle aggiornate:'
026109141202     o                       contagg          +   1
026110141202     o                                        +   2 'Bolle allocate Part.'
026111141202     o                       contlckblp       +   1
026112141202     o               51                       +   2 'Bolle allocate Arr.'
026113141202     o               51      contlckarb       +   1
026114141202     o               51                       +   2 'Bolle allocate Sede'
026115141202     o               51      contlcktas       +   1
026200970910     O*------------------------------------------------------------------------*
026300970910** MSG - MESSAGGI
026400110504Data di riferimento errata
026500141201data prima DEL deve essere > della data di spedizione
