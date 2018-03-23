000100041018     H*---------------------------------------------------------------*
000200160222     H* FNLVDPR       Verifica possibilità di instradare da EEX a DPD *
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
005200041018     C*---------------------------------------------------------------*
005300041018     C* MAIN LINE                                                     *
005400041018     C*---------------------------------------------------------------*
005500130621     c     *entry        plist
005600130621     c                   parm                    kpjba
005700130621     c                   parm                    ds_fnblp
005800130621     c                   movel     kpjbu         fnlvemds
005900130624
006000130624     C* TIPO LANCIO: "C" -           CHIUSO CON LR
006100130624     C* TIPO LANCIO: "L" - ELABORO E CHIUDO CON LR
006200130624     C* TIPO LANCIO: " " - ELABORO E CHIUDO IN RETRN
006300130624    0c                   if        iemtla<>'C'
006400130624     c* pulizia campi di output
006500130624     c                   clear                   oemesito
006600130624     c                   clear                   ds_fnblp
006700151105     c
006800130621     c                   clear                   wsputrov
006900130621     c* Con la key sped. ricevuta chaino la testata bolla
007000130621     c     kblp          chain     fnblp01l
007100130624    1c                   if        %found(fnblp01l)
007200151110     c
007300151110     c                   clear                   wbsptp
007400151105
007500151105     c* Per alcune condizioni posticipo alert in chiusura FGV
007600151105     c                   exsr      ctr_POST
007700151105     c
007800151105   1ac                   if        blpft1=*blanks and wpost=' '
007900151110
008000151110     c* Verifico se bolla che può partire da più filiali
008100151110     c                   exsr      ctr_BSP
008200151110     c* se PARTE da più filiali
008300151110     c*  leggo dettaglio colli e invio alert solo se aggiornata la data
008400151110     c*  di entrata
008500151110   1bc                   if        Wbsptp<>' '
008600151110     c
008700151110    2c                   if        blpdse>0
008800151110     c                   eval      wsputrov='S'
008900151110    2c                   endif
009000151110     c
009100151110  x1bc                   else
009200151110     c
009300151110     c* altrimenti cerco una spunta per inviare alert
009400130621     c* colli non sequenziali
009500130621    2c                   if        blpfns='S'
009600130621     c     kblt          setll     fnblt01l
009700130621    3c                   do        *hival
009800130621     c     kblt          reade     fnblt01l
009900130621    4c                   if        %eof(fnblt01l)
010000130621     c                   leave
010100130621    4c                   endif
010200130621     c                   z-add     bltnsc        wncl
010300130621     c                   exsr      Sr_cerspu
010400130621     c                   if        wsputrov='S'
010500130621     c                   leave
010600130621     c                   endif
010700130621    3c                   enddo
010800130621   x2c                   else
010900130621     c* colli sequenziali
011000130621     c                   z-add     blpncd        wncl
011100130621    3c     blpnca        ifeq      0
011200130621     c                   z-add     blpncd        blpnca
011300130621    3c                   endif
011400130621    3c     wncl          dowle     blpnca
011500130621     c                   exsr      Sr_cerspu
011600130621     c                   if        wsputrov='S'
011700130621     c                   leave
011800130621     c                   endif
011900130621    4c                   if        wncl=9999999
012000130621     c                   leave
012100130621    4c                   endif
012200130621     c                   add       1             wncl
012300130621    3c                   enddo
012400130621    2c                   endif
012500151110   1bc                   endif
012600151110   1ac                   endif
012700151110
012800151105     c* Avviso al destinatario inviabile se :
012900151105     c* - bolla     partita senza controllo di spunta partenza
013000151105     c* - bolla     partita con   controllo di spunta partenza
013100151105     c* - bolla non partita e almeno una spunta entrata o partenza
013200151110     c* - bolla non partita e data entrata aggiornata, se parte da più fil
013300151105     c                   if        (blpft1='T' and wpost=' ') or
013400151105     c                             wpost='S'  or (wsputrov='S' and blpft1=' ')
013500130624     c                   eval      oemesito='1'
013600130624     c                   eval      kpjbu=fnlvemds
013700130624     c                   endif
013800130621    1c                   endif
013900130624     c*
014000130624    0c                   endif
014100130624
014200130624    1C     iemtla        IFEQ      ' '
014300130624     C                   RETURN
014400130624     C                   ELSE
014500041022     c                   seton                                        lr
014600130624     c                   endif
014700041021     c**********************************************************************
014800130621     c     Sr_cerspu     begsr
014900041021     c                   clear                   wsputrov
015000070117     c     kbrv          setll     fnbrv07l
015100041021     c                   do        *hival
015200070117     c     kbrv          reade     fnbrv07l
015300041021     c* esco dal ciclo per fine file o per spunta trovata
015400070117     c                   if        %eof(fnbrv07l) or wsputrov = 'S'
015500041021     c                   leave
015600041021     c                   endif
015700130621     c* considero solo spunte di categoria 5 e 1
015800130621     c                   if        brvnpg=5 or brvnpg=1
015900041021     c                   movel     'S'           wsputrov
016000041021     c                   endif
016100041021     c                   enddo
016200041021     c                   endsr
016300151105     c**********************************************************************
016400151105     c     Carica_post   begsr
016500151110     c                   open      tntbe01l
016600151105     c* Lettura tabelle EMD
016700151105     c* Per tipo servizio
016800151105     c                   eval      xx=1
016900151105     c                   eval      zz=1
017000151105     c                   eval      kcod='EMD'
017100151105     c
017200151105     c                   eval      kke1='TSP'
017300151105     c     ktbe          setll     tntbe01l
017400151105     c     ktbe          reade     tntbe01l
017500151105     c
017600151105     c                   dow        not %eof(tntbe01l)
017700151105     c                   if        tbeatb=' '
017800151105     c                   movel     tbeke2        tsp(xx)
017900151105     c                   add       1             xx
018000151105     c                   endif
018100151105     c
018200151105     c     ktbe          reade     tntbe01l
018300151105     c                   enddo
018400151105     c
018500151105     c* Per terminal partenza/arrivo
018600151105     c                   eval      kke1='TFPTFA'
018700151105     c     ktbe          setll     tntbe01l
018800151105     c     ktbe          reade     tntbe01l
018900151105     c
019000151105     c                   dow        not %eof(tntbe01l)
019100151105     c                   if        tbeatb=' '
019200151105     c                   movel     tbeke2        tfptfa(zz)
019300151105     c                   add       1             zz
019400151105     c                   endif
019500151105     c
019600151105     c     ktbe          reade     tntbe01l
019700151105     c                   enddo
019800151105     c
019900151105     c* Carica dalla tabella CLI i clienti che posticipano
020000151105     c                   eval      kcod='CLI'
020100151105     c                   eval      zz=1
020200151105     c
020300151105     c     kcod          setll     tntbe01l
020400151105     c     kcod          reade     tntbe01l
020500151105     c                   dow        not %eof(tntbe01l)
020600151105     c                   if        tbeatb=' '
020700151105     c                   eval      dcli=tbeuni
020800151105     c                   if        §cliemdp='S'
020900151105     c                   movel     tbeke1        cli(zz)
021000151105     c                   add       1             zz
021100151105     c                   endif
021200151105     c                   endif
021300151105
021400151105     c     kcod          reade     tntbe01l
021500151105     c                   enddo
021600151105     c
021700151110     c                   close     tntbe01l
021800151105     c                   endsr
021900151110     c**********************************************************************
022000151110     c     Carica_bsp    begsr
022100151110     c* Per cliente
022200151110     c                   eval      xx=1
022300151110     c                   eval      zz=1
022400151110     c                   open      fibsp02l
022500151110     c
022600151110     c                   clear                   wccm
022700151110     c     wccm          setgt     fibsp02l
022800151110     c                   read      fibsp02l
022900151110     c
023000151110     c                   dow        not %eof(fibsp02l)
023100151110     c                   if        bspksc>0 and bspfpa='S'
023200151110     c                   movel     bspksc        bksc(xx)
023300151110     c                   add       1             xx
023400151110     c                   endif
023500151110     c
023600151110     c                   read      fibsp02l
023700151110     c                   enddo
023800151110     c
023900151110     c                   open      fibsp01l
024000151110     c
024100151110     c                   clear                   w_lnpbo
024200151110     c     w_lnpbo       setgt     fibsp01l
024300151110     c                   read      fibsp01l
024400151110     c
024500151110     c                   dow       not %eof(fibsp01l)
024600151110     c                   if        bspfil>0 and bspfpa='S'
024700151110     c                   movel     bspfil        blnp(zz)
024800151110     c                   add       1             zz
024900151110     c                   endif
025000151110     c
025100151110     c                   read      fibsp01l
025200151110     c                   enddo
025300151110     c                   close     fibsp02l
025400151110     c                   close     fibsp01l
025500151110     c                   ENDSR
025600151105     c**********************************************************************
025700151105     c     Ctr_post      begsr
025800151105     c
025900151105     c                   clear                   wpost
026000151105     c
026100151105     c                   if        %lookup(blptsp:tsp)>0
026200151106     c                   eval      wpost='N'
026300151105     c                   endif
026400151105
026500151105     c                   if        wpost=' '
026600151105     c                   z-add     blptfa        w0060             6 0
026700151105     c                   movel     blptfp        w0060             6 0
026800151105     c                   if        %lookup(w0060:tfptfa)>0
026900151105     c                   eval      wpost='N'
027000151105     c                   endif
027100151105     c                   endif
027200151105
027300151105     c                   if        wpost=' '
027400151105     c                   if        blpccm>0
027500151105     c                   eval      wccm=blpccm
027600151105     c                   else
027700151105     c                   eval      wccm=blpksc
027800151105     c                   endif
027900151105     c                   if        %lookup(wccm:cli)>0
028000151105     c                   eval      wpost='N'
028100151105     c                   endif
028200151105     c                   endif
028300151105     c
028400151105
028500151105     c* Se posticipato controllo la presenza della spunta partenza
028600151105     c*  TRASMESSA e bolla TRASMESSA
028700151105     c                   if        wpost='N'
028800151105     c                   if        blpft1='T' and blpdpc>0
028900151105     c                   eval      wpost='S'
029000151105     c                   endif
029100151105     c                   endif
029200151105     c
029300151105     c                   endsr
029400151110     c**********************************************************************
029500151110     c     Ctr_BSP       begsr
029600151110     c                   eval      WBspTp=*blanks
029700151110    2c                   if        blpccm>0
029800151110     c                   eval      wccm=blpccm
029900151110     c                   else
030000151110     c                   eval      wccm=blpksc
030100151110    2c                   endif
030200151110     c
030300151110     c                   if        %lookup(Wccm:bksc)>0
030400151110     c                   eval      wBspTp='K'
030500151110     c                   else
030600151110     c                   if        %lookup(blplnp:blnp)>0
030700151110     c                   eval      wBspTp='L'
030800151110     c                   endif
030900151110     c                   endif
031000151110
031100151110     c                   endsr
031200041018     c**********************************************************************
031300041018     c     *inzsr        begsr
031400041022     c*
031500050419     c*
031600050419     c     *dtaara       define    §azute        azuteds
031700050419     c     *dtaara       define    §datiute      ddatiute
031800050419     c                   in(E)     *dtaara
031900050419     c                   If        %error  or RSUT = *blanks
032000050419     c                   CLEAR                   tibs34ds
032100050419     c                   CALL      'TIBS34R'
032200050419     c                   PARM                    tibs34ds
032300050419     c                   in        *dtaara
032400050419     c                   EndIf
032500041018     c
032600130624     c     kblp          klist
032700130624     c                   kfld                    iemaas
032800130624     c                   kfld                    iemlnp
032900130624     c                   kfld                    iemnrs
033000130624     c                   kfld                    iemnsp
033100041021     c     kbrv          klist
033200041025     c                   kfld                    blpfls
033300041021     c                   kfld                    blplna
033400041021     c                   kfld                    blpnrs
033500041021     c                   kfld                    wncl
033600041021     c     kblt          klist
033700041021     c                   kfld                    blpaas
033800041021     c                   kfld                    blplnp
033900041021     c                   kfld                    blpnrs
034000041021     c                   kfld                    blpnsp
034100151105     c     KTBE          klist
034200151105     c                   kfld                    kcod
034300151105     c                   kfld                    kke1
034400151105     c
034500151105     c                   exsr      Carica_Post
034600151110     c                   exsr      Carica_BSP
034700041018     c
034800041021     c                   z-add     1             codut             1 0
034900041021     C**  CARICO TABELLA 7J - CODICI PISTOLA delle pistole da non considerar
035000130624     C*                  MOVEL     '7J'          KCOD
035100130624     C*                  Z-ADD     1             X                 4 0
035200130624     C*    KTAB2         SETLL     TABEL
035300130624     C*    KTAB2         READE     TABEL                                  99
035400041021     C*
035500130624    2C*    *IN99         DOWEQ     *OFF
035600130624    3C*    TBLFLG        IFEQ      ' '
035700130624     C*                  MOVEL     TBLUNI        DS7J
035800130624     c*    §7jrps        ifeq      'A'
035900130624     C*                  MOVEL     TBLKEY        NPS(X)
036000130624     C*                  ADD       1             X
036100130624     c*                  endif
036200130624    3C*                  END
036300041021     C*
036400130624     C*    KTAB2         READE     TABEL                                  99
036500130624    2C*                  ENDDO
036600050419     c*
036700050418     c                   endsr
