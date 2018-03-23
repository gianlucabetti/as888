000100970910     H*------------------------------------------------------------------------*
000200970910     H DECEDIT('0,') DATEDIT(*DMY.)
000300970910     F*------------------------------------------------------------------------*
000400970910     F* DATA BASE
000500970910     F*------------------------------------------------------------------------*
000600171003     Ffnlsy1d   CF   E             WORKSTN usropn
000601171003     Fazorg01l  if   E           k disk
000700141201     Ffnblp25l  if   E           k disk
000800141201     F                                     INFDS(FNblp1)
000900141201     F                                     RENAME(FNblp000:FNblp025)
000901171003     Ffnblp31l  if   E           k disk
000902171003     F                                     INFDS(FNblp31)
000903171003     F                                     RENAME(FNblp000:FNblp031)
001000141201     FFNblp00F  UF   E             DISK
001001141201     Ffnarb01l  uf   E           k disk    usropn
001002141201     Ftitas30c  uf   E           k disk    extfile(wlibsede) usropn
001100141201     Ftabel00f  if   E           k disk
001101141202     Ffnlbl01l  if   E           k disk
001102141202     fqsysprt   o    F  132        printer
001200060421     d
001300171003     D MSG             S             78    DIM(8) CTDATA PERRCD(1)
001500970910     D*------------
001600141201     D fnblp1          DS
001700141201     D  blpNRR               397    400B 0
001701171003     D fnblp31         DS
001702171003     D  blpNR1               397    400B 0
001703171003     D                 DS
001800171003     d  vlna1
001801171003     d  vlna2
001802171003     d  vlna3
001803171003     d  vlna4
001804171003     d  vlna5
001805171003     d  vlna6
001806171003     d  vlna7
001807171003     d  vlna8
001808171003     d  vlna9
001809171003     d  vlna10
001810171003     d  vlna11
001811171003     d  vlna12
001812171003     d  vlna13
001813171003     d  vlna14
001814171003     d  vlna15
001815171003     d lna                     1     45s 0 dim(15) inz
001816171003
001900970910     D KPJBA         E DS
002000060421     d Azuteds       e ds                  extname(Azute00f)
002100060421     d dDatiute      e ds
002200141201     d ds3a          e ds
002300060421     D TIBS34DS      E DS
002301141204     D xgiolavds     E DS
002302171003     d TIBS69DS      E DS                  INZ
002303171003     d DS_cnaco      E DS                  extname(CNACO00F)
002304171003     d DS_cnind      E DS                  extname(CNIND00F)
002305171003     d DS_cnclp      E DS                  extname(CNCLP00F)
002306171003     d DS_fncls      E DS                  extname(FNCLS00F)
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
003006171003     d i               s              2  0
003007171003     d ii              s              2  0
003008171003     d wlna            s              1
003009171003     d wnrr            s                    like(blpnrr)
003100141126     d Datasys         s               d   datfmt(*iso)  inz(*sys)
003101141201     D Wlibsede        S             21
003102141201     D WlibsedeP       S             21    inz('GAITRAGRPS/TITAS30C')
003103141201     D WlibsedeB       S             21    INZ('GAITRAGRU /TITAS30C')
003200060421     d
003201171003      /copy gaitrasrc/srcprotopr,tibs69r
003202171003
003203141202     D titolo          C                   CONST('Aggiornamento Data -
003204171003     D                                     Consegna Richiesta bolle cliente')
003206141201     ITITAS000      02
003207141201     ITITAS010      03
003208141201     ITITASP00      04
003300970910     C*------------------------------------------------------------------------*
003400970910     C*
003500060420     c* Impostazioni iniziali
003600141126     c                   eval      dataiso=datasys
003700141126     c                   eval      dataeur=dataiso
003800141126     c                   eval      dateu  =%dec(dataiso)
003900141126     c                   eval      v1cdri =%dec(dataeur)
003901141202     c                   eval      wdata  =%dec(dataeur)
004000171003     c                   eval      vlna1=0
004100171003     c                   eval      vlna2=0
004200171003     c                   eval      vlna3=0
004300171003     c                   eval      vlna4=0
004400171003     c                   eval      vlna5=0
004500171003     c                   eval      vlna6=0
004600171003     c                   eval      vlna7=0
004700171003     c                   eval      vlna8=0
004800171003     c                   eval      vlna9=0
004900171003     c                   eval      vlna10=0
005000171003     c                   eval      vCLI  =0
005002141204     c* se pgm non schedulato propongo a video data "Prima Del"=data odierna
005003141204     c* + 3gg lavorativi  ed emetto il video
005004141204    1c                   if        pschedul=' '
005005141204     c                   clear                   xgiolavds
005006141204     c                   eval      ixgldata=dateu
005007171003     c                   eval      ixglfil =simfel
005008171006     c                   eval      ixglpa  ='P'
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
005500171003     c                   setoff                                       414243
005501171003     c                   setoff                                       444546
005502171003     c                   setoff                                       474849
005503171003     c                   setoff                                       505152
005504171003     c                   setoff                                       535455
005505171003     c                   setoff                                       565758
005506171003     c                   setoff                                       5960
005600060421     c
005700060421     c* F3 - Fine
005800060421     c   kc              goto      fine
005900970910     C*
006000060421     c* controlli
006100060421     c                   EXSR      CTRD01
006101171003     c                   if        vgg>0
006102171003     c                   goto      for01
006103171003     c                   endif
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
007801171003     c* controllo il codice cliente
007802171003        if vcli = 0;
007803171003          *in43=*on  ;
007804171003          *in28=*on  ;
007805171003          *in90=*on  ;
007806171003          v1cmsg=MSG(3) ;
007807171003          leavesr;
007808171003        endif;
007809171003           clear tibs69ds;
007810171003           clear DS_cnaco;
007811171003           clear DS_cnind;
007812171003           clear DS_cnclp;
007813171003           clear DS_fncls;
007814171003           I69kac=vcli           ;
007815171003           tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
007816171003              if o69err<>*blanks;
007817171003                 v1cmsg = msg(03);
007818171003                 *in28 = *on;
007819171003                 *in43 = *on;
007820171003                 *in90 = *on;
007823171003                 leavesr;
007824171003              endif;
007825171003                // decodifico il codice cliente
007826171003              vdcli=acorag;
007827171003        // controlla  la linea di partenza se immessa
007828171003        if vlnp>0;
007829171003           chain vlnp azorg01l ;
007830171003           if not %found(azorg01l);
007831171003                  eval *in60=*on;
007832171003                  eval *in28=*on;
007833171003                  eval *in90=*on;
007834171003                  v1cmsg = msg(07);
007835171003                  leavesr;
007836171003           endif;
007837171003        endif;
007838171003        // Controllo le linee di arrivo se immesse
007839171003        eval ii=45 ;
007840171003        clear wlna;
007841171003        for I=1 to %elem(lna);
007842171003           if lna(i)>0;
007843171003              chain lna(I) azorg01l;
007844171003              if not %found(azorg01l);
007845171003                  eval *in(ii)=*on;
007846171003                  eval *in28=*on;
007847171003                  eval *in90=*on;
007848171003                  v1cmsg = msg(04);
007849171003                  leavesr;
007850171003              endif;
007851171003              wlna='S';
007852171003           endif;
007853171003           ii+=1;
007854171003        endfor;
007855171003     c* errore se manca sia lnp che lna
007856171003        if vlnp=0 and wlna=*blanks;
007857171003                  eval *in60=*on;
007858171003                  eval *in28=*on;
007859171003                  eval *in90=*on;
007860171003                  v1cmsg = msg(08);
007861171003                  leavesr;
007862171003        endif;
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
009501171003     c* inserire la data OPPURE il numero dei giorni
009502171003     c                   if        (vprima>0 and vgg>0) or
009503171003     c                             (vprima=0 and vgg=0)
009504171003
009505171003     c                   seton                                        422890
009506171003     C                   MOVEL     MSG(5)        V1cMSG
009507171003     c                   goto      endct1
009508171003    1c                   endif
009600141201     c* PRIMA DEL
009601171003     c                   if        vprima > 0
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
011202171003     c                   endif
011203171003     c* controllo +gg
011204171003     c                   if        vgg>0 and vgg>4
011205171003     c                   seton                                        442890
011206171003     C                   MOVEL     MSG(6)        V1cMSG
011207171003     c                   goto      endct1
011208171003     c                   endif
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
015604171003     c                   open      fnlsy1d
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
016901171003     c     kblp          klist
016902171003     c                   kfld                    klna              3 0
016903171003     c                   kfld                    kdcm              8 0
016904171003     c                   kfld                    kaas              4 0
016905171003     c                   kfld                    kmgs              4 0
016906171003     c     kblp31        klist
016907171003     c                   kfld                    VLNP              3 0
016908171003     c                   kfld                    kaas              4 0
016909171003     c                   kfld                    kmgs              4 0
016910171003     c     ktab          klist
016911171003     c                   kfld                    codut             1 0
016912171003     c                   kfld                    kcod              2
016913171003     c                   kfld                    kkey              8
016914171003     c                   clear                   kdcm
016915171003     c     kbolla        klist
016916171003     c                   kfld                    blpaas
016917171003     c                   kfld                    blplnp
016918171003     c                   kfld                    blpnrs
016919171003     c                   kfld                    blpnsp
017000060421     c                   ENDSR
017100141201     C*------------------------------------------------------------------------*
017200141201     C     ELABORA       BEGSR
017220141202     c                   setoff                                       51
017221141201     c                   if        %subst(knmus:1:3)='EDP'
017222141202     c                   seton                                        51
017223141201     c                   open      fnarb01l
017224141201      /free
017225141201       if %subst(knsif:7:1) = 'P';
017226141201         Wlibsede = Wlibsedep;
017227141201         Else ;
017228141201         wlibsede = Wlibsedeb;
017229141201       endif;
017230141201       Open   Titas30c;
017231141201      /end-free
017232141201     c                   endif
017300141201     c                   z-add     1             XX                3 0
017400171003     c*                  z-add     vlna1         lna(1)
017500171003     c*                  z-add     vlna2         lna(2)
017600171003     c*                  z-add     vlna3         lna(3)
017700171003     c*                  z-add     vlna4         lna(4)
017800171003     c*                  z-add     vlna5         lna(5)
017900171003     c*                  z-add     vlna6         lna(6)
018000171003     c*                  z-add     vlna7         lna(7)
018100171003     c*                  z-add     vlna8         lna(8)
018200171003     c*                  z-add     vlna9         lna(9)
018300171003     c*                  z-add     vlna10        lna(10)
018400171003     c*                  z-add     vlna11        lna(11)
018500171003     c*                  z-add     vlna12        lna(12)
018600171003     c*                  z-add     vlna13        lna(13)
018700171003     c*                  z-add     vlna14        lna(14)
018800171003     c*                  z-add     vlna15        lna(15)
018801171003      *?Elaborazione per LNA
018900171003     c                   if        wlna='S'
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
020400141201     c
020500141201     C* LEGGO LE BOLLE PARTENZA DEL CLIENTE
020600141201     c     kblp          setll     fnblp25l
020700141201     c     kblp          reade     fnblp25l
020800141201     c
020900141201     c                   dow       not %eof(fnblp25l)
020901171003
020902171003     c* scarto se lnp non corrisponde con quella eventualmente immess
020903171003     C                   IF        VLNP>0 AND VLNP<>BLPLNP
020904171003     c     kblp          reade     fnblp25l
020905171003     c                   iter
020906171003     C                   ENDIF
020907171003
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
023801171003     c                   z-add     blpnrr        wnrr
023900141201     c                   exsr      Aggiorna
024000141201     c                   endif
024100141201     c
024200141201     c     kblp          reade     fnblp25l
024300141201     c                   enddo
024400141201     c
024500141201     c                   add       1             xx                3 0
024600141201     c                   enddo
024601171003     c                   else
024602171003     c                   movel     amgdri        kaas
024603171003     c                   move      amgdri        kmgs
024604171003      *?Elaborazione per LNp
024605171003     C     kblp31        SETLL     fnblp31l
024606171003     c     kblp31        reade     fnblp31l
024607171003    1c                   dow       not %eof(fnblp31l)
024608171003     c                   select
024609171003     c                   when      blpccm>0  and blpccm<>vcli
024610171003     c     kblp31        reade     fnblp31l
024611171003     c                   iter
024612171003     c
024613171003     c                   when      blpccm=0 and blpksc<>vcli
024614171003     c     kblp31        reade     fnblp31l
024615171003     c                   iter
024616171003     c                   endsl
024617171003     c* scarto se consegnata
024618171003     c                   if        blpdcm>0
024619171003     c     kblp31        reade     fnblp31l
024620171003     c                   iter
024621171003     c                   endif
024622171003
024623171003     c* scarto se bolla figlia
024624171003     c     kbolla        setll     fnlbl01l
024625171003     c                   if        %equal(fnlbl01l)
024626171003     c     kblp31        reade     fnblp31l
024627171003     c                   iter
024628171003     c                   endif
024629171003     c
024630171003     c* escludo bolle recupero
024631171003     c                   z-add     1             codut
024632171003     c                   clear                   ds3a
024633171003     c                   eval      kcod='3A'
024634171003     c                   eval      kkey=blpcbo
024635171003     c     ktab          chain     tabel
024636171003     c                   if        %found(tabel00f )
024637171003     c                   movel     tbluni        ds3a
024638171003     c* escluse le recupero
024639171003     c                   endif
024640171003     c
024641171003     c                   if        §3arbl<>'R'
024642171003     c* se non è EDP solo bolle da partire
024643171003     c                   if        %subst(knmus:1:3)<>'EDP'
024644171003     c* se già partita non aggiorno
024645171003     c                   if        blpnfv>0 and blpdbr>0 and blpft1<> ' '
024646171003     c     kblp31        reade     fnblp31l
024647171003     c                   iter
024648171003     c                   endif
024649171003     c                   endif
024650171003     c
024651171003     c* aggiorno
024652171003     c                   z-add     blpnr1        wnrr
024653171003     c                   exsr      Aggiorna
024654171003     c                   endif
024655171003     c     kblp31        reade     fnblp31l
024656171003    1c                   enddo
024657171003    0c                   endif
024700141201     c
024800141201     c                   ENDSR
024900141201     O*------------------------------------------------------------------------*
025000141201     C     AGGIORNA      BEGSR
025100171003     C     wnrr          CHAIN(e)  FNBLP00f
025200141201    1c                   if        %error
025201141202     c                   eval      contlckblp+=1
025300141201   x1c                   else
025400171006    2c                   if        blpdcr=0 OR blptcr='P'
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
025807171006    5c                   if        arbdcr=0 or arbtcr='P'
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
025818171006    5c                   if        tasdcr=0  or tastcr='P'
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
026104171003     o                       vcli          z  +   1
026105171003     o                                        +   1 '"Prima DEL'
026106141202     o                       vprima           +   1 '  /  /    '
026107141202     o                                        +   1 '"'
026108141202     O                       wdata              132 '  /  /    '
026109141202     o          e            p_tot       2
026110141202     o                                              'Bolle aggiornate:'
026111141202     o                       contagg          +   1
026112141202     o                                        +   2 'Bolle allocate Part.'
026113141202     o                       contlckblp       +   1
026114141202     o               51                       +   2 'Bolle allocate Arr.'
026115141202     o               51      contlckarb       +   1
026116141202     o               51                       +   2 'Bolle allocate Sede'
026117141202     o               51      contlcktas       +   1
026200970910     O*------------------------------------------------------------------------*
026300970910** MSG - MESSAGGI
026400110504Data di riferimento errata
026500141201data prima DEL deve essere > della data di spedizione
026501171003Codice cliente mancante o errato
026502171003linea di arrivo errata
026600171003Inserire la data "Prima Del" oppure il numero di giorni da aggiungere
0267001710034 giorni è il numero massimo amesso
026800171003Linea di partenza errata
026900171003Linea di partenza o linea di arrivo obbligatoria
