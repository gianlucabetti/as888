000300041018     H*---------------------------------------------------------------*
000400130624     H* Verifica presenza spunte Entr/part per avviso di affidam.sped *
000500041018     H*---------------------------------------------------------------*
000600041018     H* INDICATORI USATI                                              *
000700041018     H*---------------------------------------------------------------*
000800041022     H* 30 - lookup
000900041022     H* 99 - comodo
001000041018     F*---------------------------------------------------------------*
001100041018     F* DATA BASE                                                     *
001200041018     F*---------------------------------------------------------------*
001300041018     H DECEDIT('0,') DATEDIT(*DMY.)
001400050418     F*
001600130621     FFNBLP01L  IF   E           K DISK
001601130621     FFNBLT01L  IF   E           K DISK
001700070117     Ffnbrv07l  IF   E           K DISK
002200041018
003700041021     d wsputrov        s              1
003701130621     D wncl            S                   LIKE(bltnsc) inz
003800130624     D*NPS             S              2  0 DIM(101)
004200041018     D*------------------
004300041018     D* ARCHITETTURA
004400041018     D*------------------
004500041018     D KPJBA         E DS
004600041018     D*------------------
004700041018     D* PARAMETRI LANCIO
004800041018     D*------------------
005700041021     d*
005800041021     D DS7J          E DS
005801130621     D fnlvemds      E DS
005802130621     d Azuteds       e ds                  Extname(Azute00f)
006300050419     d dDatiute      e ds
006400050419     d Tibs34ds      e ds                  Inz
006401130624     d ds_fnblp      e ds                  extname(fnblp00f)
006500041018     C*---------------------------------------------------------------*
006600041018     C* MAIN LINE                                                     *
006700041018     C*---------------------------------------------------------------*
006701130621     c     *entry        plist
006702130621     c                   parm                    kpjba
006703130621     c                   parm                    ds_fnblp
006704130621     c                   movel     kpjbu         fnlvemds
006705130624
006706130624     C* TIPO LANCIO: "C" -           CHIUSO CON LR
006707130624     C* TIPO LANCIO: "L" - ELABORO E CHIUDO CON LR
006708130624     C* TIPO LANCIO: " " - ELABORO E CHIUDO IN RETRN
006709130624    0c                   if        iemtla<>'C'
006710130624     c* pulizia campi di output
006711130624     c                   clear                   oemesito
006712130624     c                   clear                   ds_fnblp
006713130621
006714130621     c
006715130621     c                   clear                   wsputrov
007200130621     c* Con la key sped. ricevuta chaino la testata bolla
007201130621     c     kblp          chain     fnblp01l
007203130624    1c                   if        %found(fnblp01l)
007204130624   1ac                   if        blpft1=*blanks
007205130621     c* colli non sequenziali
007206130621    2c                   if        blpfns='S'
007207130621     c     kblt          setll     fnblt01l
007208130621    3c                   do        *hival
007209130621     c     kblt          reade     fnblt01l
007210130621    4c                   if        %eof(fnblt01l)
007211130621     c                   leave
007212130621    4c                   endif
007213130621     c                   z-add     bltnsc        wncl
007214130621     c                   exsr      Sr_cerspu
007215130621     c                   if        wsputrov='S'
007216130621     c                   leave
007217130621     c                   endif
007218130621    3c                   enddo
007219130621   x2c                   else
007220130621     c* colli sequenziali
007221130621     c                   z-add     blpncd        wncl
007222130621    3c     blpnca        ifeq      0
007223130621     c                   z-add     blpncd        blpnca
007224130621    3c                   endif
007225130621    3c     wncl          dowle     blpnca
007226130621     c                   exsr      Sr_cerspu
007227130621     c                   if        wsputrov='S'
007228130621     c                   leave
007229130621     c                   endif
007230130621    4c                   if        wncl=9999999
007231130621     c                   leave
007232130621    4c                   endif
007233130621     c                   add       1             wncl
007234130621    3c                   enddo
007235130621    2c                   endif
007236130624   1ac                   endif
007237130624     c* Avviso al destinatario inviabile se bolla partita o presente
007238130624     c* almeno una spunta entrata o partenza
007239130624     c                   if        blpft1<>*blanks or wsputrov='S'
007240130624     c                   eval      oemesito='1'
007241130624     c                   eval      kpjbu=fnlvemds
007242130624     c                   endif
007243130621    1c                   endif
007244130624     c*
007248130624    0c                   endif
007249130624
007250130624    1C     iemtla        IFEQ      ' '
007251130624     C                   RETURN
007252130624     C                   ELSE
007300041022     c                   seton                                        lr
007301130624     c                   endif
028400041021     c**********************************************************************
028500130621     c     Sr_cerspu     begsr
028600041021     c                   clear                   wsputrov
028700070117     c     kbrv          setll     fnbrv07l
028800041021     c                   do        *hival
028900070117     c     kbrv          reade     fnbrv07l
029000041021     c* esco dal ciclo per fine file o per spunta trovata
029100070117     c                   if        %eof(fnbrv07l) or wsputrov = 'S'
029200041021     c                   leave
029300041021     c                   endif
029400130621     c* considero solo spunte di categoria 5 e 1
029401130621     c                   if        brvnpg=5 or brvnpg=1
029900041021     c                   movel     'S'           wsputrov
030000041021     c                   endif
030100041021     c                   enddo
030600041021     c                   endsr
030700041018     c**********************************************************************
030800041018     c     *inzsr        begsr
030900041022     c*
031300050419     c*
031400050419     c     *dtaara       define    §azute        azuteds
031500050419     c     *dtaara       define    §datiute      ddatiute
031600050419     c                   in(E)     *dtaara
031700050419     c                   If        %error  or RSUT = *blanks
031800050419     c                   CLEAR                   tibs34ds
031900050419     c                   CALL      'TIBS34R'
032000050419     c                   PARM                    tibs34ds
032100050419     c                   in        *dtaara
032200050419     c                   EndIf
032300041018     c
032301130624     c     kblp          klist
032302130624     c                   kfld                    iemaas
032303130624     c                   kfld                    iemlnp
032304130624     c                   kfld                    iemnrs
032305130624     c                   kfld                    iemnsp
033200041021     c     kbrv          klist
033300041025     c                   kfld                    blpfls
033400041021     c                   kfld                    blplna
033500041021     c                   kfld                    blpnrs
033600041021     c                   kfld                    wncl
033700041021     c     kblt          klist
033800041021     c                   kfld                    blpaas
033900041021     c                   kfld                    blplnp
034000041021     c                   kfld                    blpnrs
034100041021     c                   kfld                    blpnsp
035200041018     c
035300041021     c                   z-add     1             codut             1 0
035400041021     C**  CARICO TABELLA 7J - CODICI PISTOLA delle pistole da non considerar
035500130624     C*                  MOVEL     '7J'          KCOD
035600130624     C*                  Z-ADD     1             X                 4 0
035700130624     C*    KTAB2         SETLL     TABEL
035800130624     C*    KTAB2         READE     TABEL                                  99
035900041021     C*
036000130624    2C*    *IN99         DOWEQ     *OFF
036100130624    3C*    TBLFLG        IFEQ      ' '
036200130624     C*                  MOVEL     TBLUNI        DS7J
036300130624     c*    §7jrps        ifeq      'A'
036400130624     C*                  MOVEL     TBLKEY        NPS(X)
036500130624     C*                  ADD       1             X
036600130624     c*                  endif
036700130624    3C*                  END
036800041021     C*
036900130624     C*    KTAB2         READE     TABEL                                  99
037000130624    2C*                  ENDDO
038100050419     c*
038200050418     c                   endsr
