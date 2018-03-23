000100041018     H*---------------------------------------------------------------*
000200130624     H* Verifica presenza spunte Entr/part per avviso di affidam.sped *
000300041018     H*---------------------------------------------------------------*
000400041018     H* INDICATORI USATI                                              *
000500041018     H*---------------------------------------------------------------*
000600041022     H* 30 - lookup
000700041022     H* 99 - comodo
000800041018     F*---------------------------------------------------------------*
000900041018     F* DATA BASE                                                     *
001000041018     F*---------------------------------------------------------------*
001100041018     H DECEDIT('0,') DATEDIT(*DMY.)
001200050418     F*
001300130621     FFNBLP01L  IF   E           K DISK
001400130621     FFNBLT01L  IF   E           K DISK
001500070117     Ffnbrv07l  IF   E           K DISK
001600151110     Ftntbe01l  IF   E           K DISK    usropn
001700151110     Ffibsp01l  IF   E           K DISK    usropn
001800151110     Ffibsp02l  IF   E           K DISK    usropn
001900151110     f                                     rename(fibsp000:fibsp002)
002000171110     FFNFGV02L  iF   E           K DISK
002100171110     FFNFGW01L  IF   E           K DISK
002200041018
002300041021     d wsputrov        s              1
002400130621     D wncl            S                   LIKE(bltnsc) inz
002500151105     D kcod            S                   LIKE(tbecod) inz
002600151105     D kke1            S                   LIKE(tbeke1) inz
002700151105     D wccm            S                   LIKE(blpksc) inz
002800151105     D wpost           S              1
002900151105     D XX              S              3  0
003000151105     D ZZ              S              3  0
003100151110     d wBspTp          s              1
003200151110     d w_lnpbo         s              3  0
003300130624     D*NPS             S              2  0 DIM(101)
003400151105     D tsp             S              1    DIM(10)
003500151105     D tfptfa          S              6  0 DIM(199)
003600151105     D cli             S              7  0 DIM(199)
003700151110     D bksc            S              7  0 DIM(199)
003800151110     D blnp            S              3  0 DIM(199)
003900041018     D*------------------
004000041018     D* ARCHITETTURA
004100041018     D*------------------
004200041018     D KPJBA         E DS
004300041018     D*------------------
004400041018     D* PARAMETRI LANCIO
004500171110     D***************************************************************
004600171110     D                 DS
004700171110     D  FGVLNA                 1      3  0
004800171110     D  FGVFFV                 4    243
004900171110     D  FGVFF2               244    450
005000171110     D  FGWFF3               451    690
005100171110     D  FGWFF4               691    900
005200171110     D  VUOTO                901    903
005300171110     D  FFV                    1    903
005400171110     D                                     DIM(301)
005500171110     D*------------------
005600041021     d*
005700041021     D DS7J          E DS
005800151105     D dcli          E DS
005900130621     D fnlvemds      E DS
006000130621     d Azuteds       e ds                  Extname(Azute00f)
006100050419     d dDatiute      e ds
006200050419     d Tibs34ds      e ds                  Inz
006300130624     d ds_fnblp      e ds                  extname(fnblp00f)
006400171103     d Dataiso         s               d   datfmt(*iso) inz(*sys)
006500171103
006600171103     D WDAT8           DS
006700171103     D  DATADA                 1      8  0
006800171103     D  DATAA                  9     16  0
006900171103     D  GGL                   17     21  0
007000171103     D xtfptfa         DS
007100171103     D  p_tfp                  1      3  0
007200171103     D  p_tfa                  4      6  0
007300041018     C*---------------------------------------------------------------*
007400041018     C* MAIN LINE                                                     *
007500041018     C*---------------------------------------------------------------*
007600130621     c     *entry        plist
007700130621     c                   parm                    kpjba
007800130621     c                   parm                    ds_fnblp
007900130621     c                   movel     kpjbu         fnlvemds
008000130624
008100130624     C* TIPO LANCIO: "C" -           CHIUSO CON LR
008200130624     C* TIPO LANCIO: "L" - ELABORO E CHIUDO CON LR
008300130624     C* TIPO LANCIO: " " - ELABORO E CHIUDO IN RETRN
008400130624    0c                   if        iemtla<>'C'
008500130624     c* pulizia campi di output
008600130624     c                   clear                   oemesito
008700130624     c                   clear                   ds_fnblp
008800151105     c
008900130621     c                   clear                   wsputrov
009000130621     c* Con la key sped. ricevuta chaino la testata bolla
009100130621     c     kblp          chain     fnblp01l
009200130624    1c                   if        %found(fnblp01l)
009300151110     c
009400151110     c                   clear                   wbsptp
009500151105
009600151105     c* Per alcune condizioni posticipo alert in chiusura FGV
009700151105     c                   exsr      ctr_POST
009800151105     c
009900151105   1ac                   if        blpft1=*blanks and wpost=' '
010000151110
010100151110     c* Verifico se bolla che può partire da più filiali
010200151110     c                   exsr      ctr_BSP
010300151110     c* se PARTE da più filiali
010400151110     c*  leggo dettaglio colli e invio alert solo se aggiornata la data
010500151110     c*  di entrata
010600151110   1bc                   if        Wbsptp<>' '
010700151110     c
010800151110    2c                   if        blpdse>0
010900151110     c                   eval      wsputrov='S'
011000151110    2c                   endif
011100151110     c
011200151110  x1bc                   else
011300151110     c
011400151110     c* altrimenti cerco una spunta per inviare alert
011500130621     c* colli non sequenziali
011600130621    2c                   if        blpfns='S'
011700130621     c     kblt          setll     fnblt01l
011800130621    3c                   do        *hival
011900130621     c     kblt          reade     fnblt01l
012000130621    4c                   if        %eof(fnblt01l)
012100130621     c                   leave
012200130621    4c                   endif
012300130621     c                   z-add     bltnsc        wncl
012400130621     c                   exsr      Sr_cerspu
012500130621     c                   if        wsputrov='S'
012600130621     c                   leave
012700130621     c                   endif
012800130621    3c                   enddo
012900130621   x2c                   else
013000130621     c* colli sequenziali
013100130621     c                   z-add     blpncd        wncl
013200130621    3c     blpnca        ifeq      0
013300130621     c                   z-add     blpncd        blpnca
013400130621    3c                   endif
013500130621    3c     wncl          dowle     blpnca
013600130621     c                   exsr      Sr_cerspu
013700130621     c                   if        wsputrov='S'
013800130621     c                   leave
013900130621     c                   endif
014000130621    4c                   if        wncl=9999999
014100130621     c                   leave
014200130621    4c                   endif
014300130621     c                   add       1             wncl
014400130621    3c                   enddo
014500130621    2c                   endif
014600151110   1bc                   endif
014700151110   1ac                   endif
014800151110
014900151105     c* Avviso al destinatario inviabile se :
015000151105     c* - bolla     partita senza controllo di spunta partenza
015100151105     c* - bolla     partita con   controllo di spunta partenza
015200151105     c* - bolla non partita e almeno una spunta entrata o partenza
015300151110     c* - bolla non partita e data entrata aggiornata, se parte da più fil
015400151105     c                   if        (blpft1='T' and wpost=' ') or
015500151105     c                             wpost='S'  or (wsputrov='S' and blpft1=' ')
015600130624     c                   eval      oemesito='1'
015700130624     c                   eval      kpjbu=fnlvemds
015800130624     c                   endif
015900130621    1c                   endif
016000130624     c*
016100130624    0c                   endif
016200130624
016300130624    1C     iemtla        IFEQ      ' '
016400130624     C                   RETURN
016500130624     C                   ELSE
016600041022     c                   seton                                        lr
016700130624     c                   endif
016800041021     c**********************************************************************
016900130621     c     Sr_cerspu     begsr
017000041021     c                   clear                   wsputrov
017100070117     c     kbrv          setll     fnbrv07l
017200041021     c                   do        *hival
017300070117     c     kbrv          reade     fnbrv07l
017400041021     c* esco dal ciclo per fine file o per spunta trovata
017500070117     c                   if        %eof(fnbrv07l) or wsputrov = 'S'
017600041021     c                   leave
017700041021     c                   endif
017800130621     c* considero solo spunte di categoria 5 e 1
017900130621     c                   if        brvnpg=5 or brvnpg=1
018000041021     c                   movel     'S'           wsputrov
018100041021     c                   endif
018200041021     c                   enddo
018300041021     c                   endsr
018400151105     c**********************************************************************
018500151105     c     Carica_post   begsr
018600151110     c                   open      tntbe01l
018700151105     c* Lettura tabelle EMD
018800151105     c* Per tipo servizio
018900151105     c                   eval      xx=1
019000151105     c                   eval      zz=1
019100151105     c                   eval      kcod='EMD'
019200151105     c
019300151105     c                   eval      kke1='TSP'
019400151105     c     ktbe          setll     tntbe01l
019500151105     c     ktbe          reade     tntbe01l
019600151105     c
019700151105     c                   dow        not %eof(tntbe01l)
019800151105     c                   if        tbeatb=' '
019900151105     c                   movel     tbeke2        tsp(xx)
020000151105     c                   add       1             xx
020100151105     c                   endif
020200151105     c
020300151105     c     ktbe          reade     tntbe01l
020400151105     c                   enddo
020500151105     c
020600151105     c* Per terminal partenza/arrivo
020700151105     c                   eval      kke1='TFPTFA'
020800151105     c     ktbe          setll     tntbe01l
020900151105     c     ktbe          reade     tntbe01l
021000151105     c
021100151105     c                   dow        not %eof(tntbe01l)
021200151105     c                   if        tbeatb=' '
021300151105     c                   movel     tbeke2        tfptfa(zz)
021400151105     c                   add       1             zz
021500151105     c                   endif
021600151105     c
021700151105     c     ktbe          reade     tntbe01l
021800151105     c                   enddo
021900151105     c
022000151105     c* Carica dalla tabella CLI i clienti che posticipano
022100151105     c                   eval      kcod='CLI'
022200151105     c                   eval      zz=1
022300151105     c
022400151105     c     kcod          setll     tntbe01l
022500151105     c     kcod          reade     tntbe01l
022600151105     c                   dow        not %eof(tntbe01l)
022700151105     c                   if        tbeatb=' '
022800151105     c                   eval      dcli=tbeuni
022900151105     c                   if        §cliemdp='S'
023000151105     c                   movel     tbeke1        cli(zz)
023100151105     c                   add       1             zz
023200151105     c                   endif
023300151105     c                   endif
023400151105
023500151105     c     kcod          reade     tntbe01l
023600151105     c                   enddo
023700151105     c
023800151110     c                   close     tntbe01l
023900151105     c                   endsr
024000151110     c**********************************************************************
024100151110     c     Carica_bsp    begsr
024200151110     c* Per cliente
024300151110     c                   eval      xx=1
024400151110     c                   eval      zz=1
024500151110     c                   open      fibsp02l
024600151110     c
024700151110     c                   clear                   wccm
024800151110     c     wccm          setgt     fibsp02l
024900151110     c                   read      fibsp02l
025000151110     c
025100151110     c                   dow        not %eof(fibsp02l)
025200151110     c                   if        bspksc>0 and bspfpa='S'
025300151110     c                   movel     bspksc        bksc(xx)
025400151110     c                   add       1             xx
025500151110     c                   endif
025600151110     c
025700151110     c                   read      fibsp02l
025800151110     c                   enddo
025900151110     c
026000151110     c                   open      fibsp01l
026100151110     c
026200151110     c                   clear                   w_lnpbo
026300151110     c     w_lnpbo       setgt     fibsp01l
026400151110     c                   read      fibsp01l
026500151110     c
026600151110     c                   dow       not %eof(fibsp01l)
026700151110     c                   if        bspfil>0 and bspfpa='S'
026800151110     c                   movel     bspfil        blnp(zz)
026900151110     c                   add       1             zz
027000151110     c                   endif
027100151110     c
027200151110     c                   read      fibsp01l
027300151110     c                   enddo
027400151110     c                   close     fibsp02l
027500151110     c                   close     fibsp01l
027600151110     c                   ENDSR
027700151105     c**********************************************************************
027800151105     c     Ctr_post      begsr
027900151105     c
028000151105     c                   clear                   wpost
028100151105     c
028200171103     c* Posticipo per tipo servizio
028300151105     c                   if        %lookup(blptsp:tsp)>0
028400151106     c                   eval      wpost='N'
028500151105     c                   endif
028600171103     c*
028700171103     c* se sono in giornata FESTIVA tutti alert posticiparti in chiusura FGV
028800171110     c* tranne se presenete FGV con lna da borderizzare
028900171103    1c                   if        wpost=' '
029000171103     c                   clear                   wdat8
029100171103     c                   clear                   xtfptfa
029200171103
029300171103     c     *iso          movel     dataiso       datada
029400171103     c     *iso          movel     dataiso       dataa
029500171103     c                   eval      p_tfp=blptfp
029600171103     c                   call      'XSRLAV8'
029700171103     c                   parm                    wdat8
029800171103     c                   parm                    xtfptfa
029900171103     c
030000171103    2c                   if         ggl=0
030100171110     c* verifico i fogli viaggio
030200171110     c                   exsr      Ctr_fgv
030300171103    2c                   endif
030400171103    1c                   endif
030500151105
030600151105     c                   if        wpost=' '
030700170120     c* TFP / TFA
030800151105     c                   z-add     blptfa        w0060             6 0
030900151105     c                   movel     blptfp        w0060             6 0
031000151105     c                   if        %lookup(w0060:tfptfa)>0
031100151105     c                   eval      wpost='N'
031200161220     c                   else
031300170120     C* 999 / TFA
031400161220     c                   movel     999           w0060
031500161220    3c                   if        %lookup(w0060:tfptfa)>0
031600161220     c                   eval      wpost='N'
031700161220   x3c                   else
031800170120     c* TFP / 999
031900161220     c                   movel     blptfp        w0060             6 0
032000161220     c                   move      999           w0060
032100161220    4c                   if        %lookup(w0060:tfptfa)>0
032200161220     c                   eval      wpost='N'
032300161220   x4c                   else
032400170120     c* lnp / TFA
032500170120     c                   z-add     blptfa        w0060
032600170120     c                   movel     blplnp        w0060
032700170120     c                   if        %lookup(w0060:tfptfa)>0
032800170120     c                   eval      wpost='N'
032900170120     c                   else
033000170120     c* TFP / lna
033100170120     c                   z-add     blplna        w0060
033200170120     c                   movel     blptfp        w0060
033300170120     c                   if        %lookup(w0060:tfptfa)>0
033400170120     c                   eval      wpost='N'
033500170120     c                   else
033600170120     c* lnp / 999
033700170120     c                   z-add     999           w0060
033800170120     c                   movel     blplnp        w0060
033900170120     c                   if        %lookup(w0060:tfptfa)>0
034000170120     c                   eval      wpost='N'
034100170120     c                   else
034200170120     c* 999 / lna
034300170120     c                   z-add     blplna        w0060
034400170120     c                   movel     999           w0060
034500170120     c                   if        %lookup(w0060:tfptfa)>0
034600170120     c                   eval      wpost='N'
034700170120     c                   else
034800170120     c* 999 / 999
034900161220     c                   z-add     999999        w0060
035000161220    5c                   if        %lookup(w0060:tfptfa)>0
035100161220     c                   eval      wpost='N'
035200161220    5c                   endif
035300161220    4c                   endif
035400161220    3c                   endif
035500151105     c                   endif
035600151105     c                   endif
035700170119     c                   endif
035800170119     c                   endif
035900170119     c                   endif
036000170119     c                   endif
036100151105
036200151105     c                   if        wpost=' '
036300151105     c                   if        blpccm>0
036400151105     c                   eval      wccm=blpccm
036500151105     c                   else
036600151105     c                   eval      wccm=blpksc
036700151105     c                   endif
036800151105     c                   if        %lookup(wccm:cli)>0
036900151105     c                   eval      wpost='N'
037000151105     c                   endif
037100151105     c                   endif
037200151105     c
037300151105
037400151105     c* Se posticipato controllo la presenza della spunta partenza
037500151105     c*  TRASMESSA e bolla TRASMESSA
037600151105     c                   if        wpost='N'
037700151105     c                   if        blpft1='T' and blpdpc>0
037800151105     c                   eval      wpost='S'
037900151105     c                   endif
038000151105     c                   endif
038100151105     c
038200151105     c                   endsr
038300171110     C********************************************************************
038400171110     C*   CTR_FGV- controlla fogli viaggio se presente la linea di arrivo da bor
038500171110     C********************************************************************
038600171110     C     CTR_FGV       BEGSR
038700171110     c                   eval      wpost='N'
038800171110     c     *iso          movel     dataiso       wdaodi            8 0
038900171110
039000171110     c     kfgv02        setll     fnfgv02l
039100171110     c     kfgv02        reade     fnfgv02l
039200171110     c                   dow       not %eof(fnfgv02l)
039300171110     c     kfgw          chain     fnfgw01l
039400171110     c                   if        not %found(fnfgw01l)
039500171110     c                   clear                   fgwff3
039600171110     c                   clear                   fgwff4
039700171110     c                   endif
039800171110
039900171110     c* faccio finta che sia borderizzata la bolla
040000171110     c                   movel     blplna        alfalna           3
040100171110     c                   if        %lookup(alfalna:ffv)>0
040200171110     c                   eval      wpost=' '
040300171110     c                   leave
040400171110     c                   endif
040500171110     c
040600171110     c     kfgv02        reade     fnfgv02l
040700171110     c                   enddo
040800171110     c
040900171110     C                   ENDSR
041000151110     c**********************************************************************
041100151110     c     Ctr_BSP       begsr
041200151110     c                   eval      WBspTp=*blanks
041300151110    2c                   if        blpccm>0
041400151110     c                   eval      wccm=blpccm
041500151110     c                   else
041600151110     c                   eval      wccm=blpksc
041700151110    2c                   endif
041800151110     c
041900151110     c                   if        %lookup(Wccm:bksc)>0
042000151110     c                   eval      wBspTp='K'
042100151110     c                   else
042200151110     c                   if        %lookup(blplnp:blnp)>0
042300151110     c                   eval      wBspTp='L'
042400151110     c                   endif
042500151110     c                   endif
042600151110
042700151110     c                   endsr
042800041018     c**********************************************************************
042900041018     c     *inzsr        begsr
043000041022     c*
043100050419     c*
043200050419     c     *dtaara       define    §azute        azuteds
043300050419     c     *dtaara       define    §datiute      ddatiute
043400050419     c                   in(E)     *dtaara
043500050419     c                   If        %error  or RSUT = *blanks
043600050419     c                   CLEAR                   tibs34ds
043700050419     c                   CALL      'TIBS34R'
043800050419     c                   PARM                    tibs34ds
043900050419     c                   in        *dtaara
044000050419     c                   EndIf
044100041018     c
044200130624     c     kblp          klist
044300130624     c                   kfld                    iemaas
044400130624     c                   kfld                    iemlnp
044500130624     c                   kfld                    iemnrs
044600130624     c                   kfld                    iemnsp
044700041021     c     kbrv          klist
044800041025     c                   kfld                    blpfls
044900041021     c                   kfld                    blplna
045000041021     c                   kfld                    blpnrs
045100041021     c                   kfld                    wncl
045200041021     c     kblt          klist
045300041021     c                   kfld                    blpaas
045400041021     c                   kfld                    blplnp
045500041021     c                   kfld                    blpnrs
045600041021     c                   kfld                    blpnsp
045700151105     c     KTBE          klist
045800151105     c                   kfld                    kcod
045900151105     c                   kfld                    kke1
046000171110     C*
046100171110     C     Kfgv02        KLIST
046200171110     C                   KFLD                    blptfp
046300171110     C                   KFLD                    wdaodi
046400171110     C     Kfgw          KLIST
046500171110     C                   KFLD                    fgvnfv
046600171110     C                   KFLD                    fgvlnp
046700171110     C*
046800151105     c
046900151105     c                   exsr      Carica_Post
047000151110     c                   exsr      Carica_BSP
047100041018     c
047200041021     c                   z-add     1             codut             1 0
047300041021     C**  CARICO TABELLA 7J - CODICI PISTOLA delle pistole da non considerar
047400130624     C*                  MOVEL     '7J'          KCOD
047500130624     C*                  Z-ADD     1             X                 4 0
047600130624     C*    KTAB2         SETLL     TABEL
047700130624     C*    KTAB2         READE     TABEL                                  99
047800041021     C*
047900130624    2C*    *IN99         DOWEQ     *OFF
048000130624    3C*    TBLFLG        IFEQ      ' '
048100130624     C*                  MOVEL     TBLUNI        DS7J
048200130624     c*    §7jrps        ifeq      'A'
048300130624     C*                  MOVEL     TBLKEY        NPS(X)
048400130624     C*                  ADD       1             X
048500130624     c*                  endif
048600130624    3C*                  END
048700041021     C*
048800130624     C*    KTAB2         READE     TABEL                                  99
048900130624    2C*                  ENDDO
049000050419     c*
049100050418     c                   endsr
