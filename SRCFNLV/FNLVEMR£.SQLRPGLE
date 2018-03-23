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
002000041018
002100041021     d wsputrov        s              1
002200130621     D wncl            S                   LIKE(bltnsc) inz
002300151105     D kcod            S                   LIKE(tbecod) inz
002400151105     D kke1            S                   LIKE(tbeke1) inz
002500151105     D wccm            S                   LIKE(blpksc) inz
002600151105     D wpost           S              1
002700151105     D XX              S              3  0
002800151105     D ZZ              S              3  0
002900151110     d wBspTp          s              1
003000151110     d w_lnpbo         s              3  0
003100130624     D*NPS             S              2  0 DIM(101)
003200151105     D tsp             S              1    DIM(10)
003300151105     D tfptfa          S              6  0 DIM(199)
003400151105     D cli             S              7  0 DIM(199)
003500151110     D bksc            S              7  0 DIM(199)
003600151110     D blnp            S              3  0 DIM(199)
003700041018     D*------------------
003800041018     D* ARCHITETTURA
003900041018     D*------------------
004000041018     D KPJBA         E DS
004100041018     D*------------------
004200041018     D* PARAMETRI LANCIO
004300041018     D*------------------
004400041021     d*
004500041021     D DS7J          E DS
004600151105     D dcli          E DS
004700130621     D fnlvemds      E DS
004800130621     d Azuteds       e ds                  Extname(Azute00f)
004900050419     d dDatiute      e ds
005000050419     d Tibs34ds      e ds                  Inz
005100130624     d ds_fnblp      e ds                  extname(fnblp00f)
005200171103     d Dataiso         s               d   datfmt(*iso) inz(*sys)
005300171103
005400171103     D WDAT8           DS
005500171103     D  DATADA                 1      8  0
005600171103     D  DATAA                  9     16  0
005700171103     D  GGL                   17     21  0
005800171103     D xtfptfa         DS
005900171103     D  p_tfp                  1      3  0
006000171103     D  p_tfa                  4      6  0
006100041018     C*---------------------------------------------------------------*
006200041018     C* MAIN LINE                                                     *
006300041018     C*---------------------------------------------------------------*
006400130621     c     *entry        plist
006500130621     c                   parm                    kpjba
006600130621     c                   parm                    ds_fnblp
006700130621     c                   movel     kpjbu         fnlvemds
006800130624
006900130624     C* TIPO LANCIO: "C" -           CHIUSO CON LR
007000130624     C* TIPO LANCIO: "L" - ELABORO E CHIUDO CON LR
007100130624     C* TIPO LANCIO: " " - ELABORO E CHIUDO IN RETRN
007200130624    0c                   if        iemtla<>'C'
007300130624     c* pulizia campi di output
007400130624     c                   clear                   oemesito
007500130624     c                   clear                   ds_fnblp
007600151105     c
007700130621     c                   clear                   wsputrov
007800130621     c* Con la key sped. ricevuta chaino la testata bolla
007900130621     c     kblp          chain     fnblp01l
008000130624    1c                   if        %found(fnblp01l)
008100151110     c
008200151110     c                   clear                   wbsptp
008300151105
008400151105     c* Per alcune condizioni posticipo alert in chiusura FGV
008500151105     c                   exsr      ctr_POST
008600151105     c
008700151105   1ac                   if        blpft1=*blanks and wpost=' '
008800151110
008900151110     c* Verifico se bolla che può partire da più filiali
009000151110     c                   exsr      ctr_BSP
009100151110     c* se PARTE da più filiali
009200151110     c*  leggo dettaglio colli e invio alert solo se aggiornata la data
009300151110     c*  di entrata
009400151110   1bc                   if        Wbsptp<>' '
009500151110     c
009600151110    2c                   if        blpdse>0
009700151110     c                   eval      wsputrov='S'
009800151110    2c                   endif
009900151110     c
010000151110  x1bc                   else
010100151110     c
010200151110     c* altrimenti cerco una spunta per inviare alert
010300130621     c* colli non sequenziali
010400130621    2c                   if        blpfns='S'
010500130621     c     kblt          setll     fnblt01l
010600130621    3c                   do        *hival
010700130621     c     kblt          reade     fnblt01l
010800130621    4c                   if        %eof(fnblt01l)
010900130621     c                   leave
011000130621    4c                   endif
011100130621     c                   z-add     bltnsc        wncl
011200130621     c                   exsr      Sr_cerspu
011300130621     c                   if        wsputrov='S'
011400130621     c                   leave
011500130621     c                   endif
011600130621    3c                   enddo
011700130621   x2c                   else
011800130621     c* colli sequenziali
011900130621     c                   z-add     blpncd        wncl
012000130621    3c     blpnca        ifeq      0
012100130621     c                   z-add     blpncd        blpnca
012200130621    3c                   endif
012300130621    3c     wncl          dowle     blpnca
012400130621     c                   exsr      Sr_cerspu
012500130621     c                   if        wsputrov='S'
012600130621     c                   leave
012700130621     c                   endif
012800130621    4c                   if        wncl=9999999
012900130621     c                   leave
013000130621    4c                   endif
013100130621     c                   add       1             wncl
013200130621    3c                   enddo
013300130621    2c                   endif
013400151110   1bc                   endif
013500151110   1ac                   endif
013600151110
013700151105     c* Avviso al destinatario inviabile se :
013800151105     c* - bolla     partita senza controllo di spunta partenza
013900151105     c* - bolla     partita con   controllo di spunta partenza
014000151105     c* - bolla non partita e almeno una spunta entrata o partenza
014100151110     c* - bolla non partita e data entrata aggiornata, se parte da più fil
014200151105     c                   if        (blpft1='T' and wpost=' ') or
014300151105     c                             wpost='S'  or (wsputrov='S' and blpft1=' ')
014400130624     c                   eval      oemesito='1'
014500130624     c                   eval      kpjbu=fnlvemds
014600130624     c                   endif
014700130621    1c                   endif
014800130624     c*
014900130624    0c                   endif
015000130624
015100130624    1C     iemtla        IFEQ      ' '
015200130624     C                   RETURN
015300130624     C                   ELSE
015400041022     c                   seton                                        lr
015500130624     c                   endif
015600041021     c**********************************************************************
015700130621     c     Sr_cerspu     begsr
015800041021     c                   clear                   wsputrov
015900070117     c     kbrv          setll     fnbrv07l
016000041021     c                   do        *hival
016100070117     c     kbrv          reade     fnbrv07l
016200041021     c* esco dal ciclo per fine file o per spunta trovata
016300070117     c                   if        %eof(fnbrv07l) or wsputrov = 'S'
016400041021     c                   leave
016500041021     c                   endif
016600130621     c* considero solo spunte di categoria 5 e 1
016700130621     c                   if        brvnpg=5 or brvnpg=1
016800041021     c                   movel     'S'           wsputrov
016900041021     c                   endif
017000041021     c                   enddo
017100041021     c                   endsr
017200151105     c**********************************************************************
017300151105     c     Carica_post   begsr
017400151110     c                   open      tntbe01l
017500151105     c* Lettura tabelle EMD
017600151105     c* Per tipo servizio
017700151105     c                   eval      xx=1
017800151105     c                   eval      zz=1
017900151105     c                   eval      kcod='EMD'
018000151105     c
018100151105     c                   eval      kke1='TSP'
018200151105     c     ktbe          setll     tntbe01l
018300151105     c     ktbe          reade     tntbe01l
018400151105     c
018500151105     c                   dow        not %eof(tntbe01l)
018600151105     c                   if        tbeatb=' '
018700151105     c                   movel     tbeke2        tsp(xx)
018800151105     c                   add       1             xx
018900151105     c                   endif
019000151105     c
019100151105     c     ktbe          reade     tntbe01l
019200151105     c                   enddo
019300151105     c
019400151105     c* Per terminal partenza/arrivo
019500151105     c                   eval      kke1='TFPTFA'
019600151105     c     ktbe          setll     tntbe01l
019700151105     c     ktbe          reade     tntbe01l
019800151105     c
019900151105     c                   dow        not %eof(tntbe01l)
020000151105     c                   if        tbeatb=' '
020100151105     c                   movel     tbeke2        tfptfa(zz)
020200151105     c                   add       1             zz
020300151105     c                   endif
020400151105     c
020500151105     c     ktbe          reade     tntbe01l
020600151105     c                   enddo
020700151105     c
020800151105     c* Carica dalla tabella CLI i clienti che posticipano
020900151105     c                   eval      kcod='CLI'
021000151105     c                   eval      zz=1
021100151105     c
021200151105     c     kcod          setll     tntbe01l
021300151105     c     kcod          reade     tntbe01l
021400151105     c                   dow        not %eof(tntbe01l)
021500151105     c                   if        tbeatb=' '
021600151105     c                   eval      dcli=tbeuni
021700151105     c                   if        §cliemdp='S'
021800151105     c                   movel     tbeke1        cli(zz)
021900151105     c                   add       1             zz
022000151105     c                   endif
022100151105     c                   endif
022200151105
022300151105     c     kcod          reade     tntbe01l
022400151105     c                   enddo
022500151105     c
022600151110     c                   close     tntbe01l
022700151105     c                   endsr
022800151110     c**********************************************************************
022900151110     c     Carica_bsp    begsr
023000151110     c* Per cliente
023100151110     c                   eval      xx=1
023200151110     c                   eval      zz=1
023300151110     c                   open      fibsp02l
023400151110     c
023500151110     c                   clear                   wccm
023600151110     c     wccm          setgt     fibsp02l
023700151110     c                   read      fibsp02l
023800151110     c
023900151110     c                   dow        not %eof(fibsp02l)
024000151110     c                   if        bspksc>0 and bspfpa='S'
024100151110     c                   movel     bspksc        bksc(xx)
024200151110     c                   add       1             xx
024300151110     c                   endif
024400151110     c
024500151110     c                   read      fibsp02l
024600151110     c                   enddo
024700151110     c
024800151110     c                   open      fibsp01l
024900151110     c
025000151110     c                   clear                   w_lnpbo
025100151110     c     w_lnpbo       setgt     fibsp01l
025200151110     c                   read      fibsp01l
025300151110     c
025400151110     c                   dow       not %eof(fibsp01l)
025500151110     c                   if        bspfil>0 and bspfpa='S'
025600151110     c                   movel     bspfil        blnp(zz)
025700151110     c                   add       1             zz
025800151110     c                   endif
025900151110     c
026000151110     c                   read      fibsp01l
026100151110     c                   enddo
026200151110     c                   close     fibsp02l
026300151110     c                   close     fibsp01l
026400151110     c                   ENDSR
026500151105     c**********************************************************************
026600151105     c     Ctr_post      begsr
026700151105     c
026800151105     c                   clear                   wpost
026900151105     c
027000171103     c* Posticipo per tipo servizio
027100151105     c                   if        %lookup(blptsp:tsp)>0
027200151106     c                   eval      wpost='N'
027300151105     c                   endif
027400171103     c*
027500171103     c* se sono in giornata FESTIVA tutti alert posticiparti in chiusura FGV
027600171103    1c                   if        wpost=' '
027700171103     c                   clear                   wdat8
027800171103     c                   clear                   xtfptfa
027900171103
028000171103     c     *iso          movel     dataiso       datada
028100171103     c     *iso          movel     dataiso       dataa
028200171103     c                   eval      p_tfp=blptfp
028300171103     c                   call      'XSRLAV8'
028400171103     c                   parm                    wdat8
028500171103     c                   parm                    xtfptfa
028600171103     c
028700171103    2c                   if         ggl=0
028800171103     c                   eval      wpost='N'
028900171103    2c                   endif
029000171103    1c                   endif
029100151105
029200151105     c                   if        wpost=' '
029300170120     c* TFP / TFA
029400151105     c                   z-add     blptfa        w0060             6 0
029500151105     c                   movel     blptfp        w0060             6 0
029600151105     c                   if        %lookup(w0060:tfptfa)>0
029700151105     c                   eval      wpost='N'
029800161220     c                   else
029900170120     C* 999 / TFA
030000161220     c                   movel     999           w0060
030100161220    3c                   if        %lookup(w0060:tfptfa)>0
030200161220     c                   eval      wpost='N'
030300161220   x3c                   else
030400170120     c* TFP / 999
030500161220     c                   movel     blptfp        w0060             6 0
030600161220     c                   move      999           w0060
030700161220    4c                   if        %lookup(w0060:tfptfa)>0
030800161220     c                   eval      wpost='N'
030900161220   x4c                   else
031000170120     c* lnp / TFA
031100170120     c                   z-add     blptfa        w0060
031200170120     c                   movel     blplnp        w0060
031300170120     c                   if        %lookup(w0060:tfptfa)>0
031400170120     c                   eval      wpost='N'
031500170120     c                   else
031600170120     c* TFP / lna
031700170120     c                   z-add     blplna        w0060
031800170120     c                   movel     blptfp        w0060
031900170120     c                   if        %lookup(w0060:tfptfa)>0
032000170120     c                   eval      wpost='N'
032100170120     c                   else
032200170120     c* lnp / 999
032300170120     c                   z-add     999           w0060
032400170120     c                   movel     blplnp        w0060
032500170120     c                   if        %lookup(w0060:tfptfa)>0
032600170120     c                   eval      wpost='N'
032700170120     c                   else
032800170120     c* 999 / lna
032900170120     c                   z-add     blplna        w0060
033000170120     c                   movel     999           w0060
033100170120     c                   if        %lookup(w0060:tfptfa)>0
033200170120     c                   eval      wpost='N'
033300170120     c                   else
033400170120     c* 999 / 999
033500161220     c                   z-add     999999        w0060
033600161220    5c                   if        %lookup(w0060:tfptfa)>0
033700161220     c                   eval      wpost='N'
033800161220    5c                   endif
033900161220    4c                   endif
034000161220    3c                   endif
034100151105     c                   endif
034200151105     c                   endif
034300170119     c                   endif
034400170119     c                   endif
034500170119     c                   endif
034600170119     c                   endif
034700151105
034800151105     c                   if        wpost=' '
034900151105     c                   if        blpccm>0
035000151105     c                   eval      wccm=blpccm
035100151105     c                   else
035200151105     c                   eval      wccm=blpksc
035300151105     c                   endif
035400151105     c                   if        %lookup(wccm:cli)>0
035500151105     c                   eval      wpost='N'
035600151105     c                   endif
035700151105     c                   endif
035800151105     c
035900151105
036000151105     c* Se posticipato controllo la presenza della spunta partenza
036100151105     c*  TRASMESSA e bolla TRASMESSA
036200151105     c                   if        wpost='N'
036300151105     c                   if        blpft1='T' and blpdpc>0
036400151105     c                   eval      wpost='S'
036500151105     c                   endif
036600151105     c                   endif
036700151105     c
036800151105     c                   endsr
036900151110     c**********************************************************************
037000151110     c     Ctr_BSP       begsr
037100151110     c                   eval      WBspTp=*blanks
037200151110    2c                   if        blpccm>0
037300151110     c                   eval      wccm=blpccm
037400151110     c                   else
037500151110     c                   eval      wccm=blpksc
037600151110    2c                   endif
037700151110     c
037800151110     c                   if        %lookup(Wccm:bksc)>0
037900151110     c                   eval      wBspTp='K'
038000151110     c                   else
038100151110     c                   if        %lookup(blplnp:blnp)>0
038200151110     c                   eval      wBspTp='L'
038300151110     c                   endif
038400151110     c                   endif
038500151110
038600151110     c                   endsr
038700041018     c**********************************************************************
038800041018     c     *inzsr        begsr
038900041022     c*
039000050419     c*
039100050419     c     *dtaara       define    §azute        azuteds
039200050419     c     *dtaara       define    §datiute      ddatiute
039300050419     c                   in(E)     *dtaara
039400050419     c                   If        %error  or RSUT = *blanks
039500050419     c                   CLEAR                   tibs34ds
039600050419     c                   CALL      'TIBS34R'
039700050419     c                   PARM                    tibs34ds
039800050419     c                   in        *dtaara
039900050419     c                   EndIf
040000041018     c
040100130624     c     kblp          klist
040200130624     c                   kfld                    iemaas
040300130624     c                   kfld                    iemlnp
040400130624     c                   kfld                    iemnrs
040500130624     c                   kfld                    iemnsp
040600041021     c     kbrv          klist
040700041025     c                   kfld                    blpfls
040800041021     c                   kfld                    blplna
040900041021     c                   kfld                    blpnrs
041000041021     c                   kfld                    wncl
041100041021     c     kblt          klist
041200041021     c                   kfld                    blpaas
041300041021     c                   kfld                    blplnp
041400041021     c                   kfld                    blpnrs
041500041021     c                   kfld                    blpnsp
041600151105     c     KTBE          klist
041700151105     c                   kfld                    kcod
041800151105     c                   kfld                    kke1
041900151105     c
042000151105     c                   exsr      Carica_Post
042100151110     c                   exsr      Carica_BSP
042200041018     c
042300041021     c                   z-add     1             codut             1 0
042400041021     C**  CARICO TABELLA 7J - CODICI PISTOLA delle pistole da non considerar
042500130624     C*                  MOVEL     '7J'          KCOD
042600130624     C*                  Z-ADD     1             X                 4 0
042700130624     C*    KTAB2         SETLL     TABEL
042800130624     C*    KTAB2         READE     TABEL                                  99
042900041021     C*
043000130624    2C*    *IN99         DOWEQ     *OFF
043100130624    3C*    TBLFLG        IFEQ      ' '
043200130624     C*                  MOVEL     TBLUNI        DS7J
043300130624     c*    §7jrps        ifeq      'A'
043400130624     C*                  MOVEL     TBLKEY        NPS(X)
043500130624     C*                  ADD       1             X
043600130624     c*                  endif
043700130624    3C*                  END
043800041021     C*
043900130624     C*    KTAB2         READE     TABEL                                  99
044000130624    2C*                  ENDDO
044100050419     c*
044200050418     c                   endsr
