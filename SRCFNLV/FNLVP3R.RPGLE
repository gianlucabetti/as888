000100110324     /*PRM tgtrls(*current)
000200110324     /*END
000300070209     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000400080328     h dftactgrp(*no) actgrp(*caller)
000500110307     h BndDir('UBBNDDIR')
000600070209      *------------------------------------------------------------------------*
000700070209      *
000800080327      * Interrogazione Log pda consegne
000900070209      *
001000070209      *------------------------------------------------------------------------*
001100080327     fazorg01l  if   e           k disk
001200080327     ffiapd01l  if   e           k disk
001300110307     ffidst01l  if   e           k disk    extfile(wLibFIDST)  usropn
001400110324     f                                     extdesc('FILTRAPRD/FIDST01L')
001500080327     ffiplg00f  if   e             disk
001600080327     ffnlvp3d   cf   e             workstn
001700070209      *------------------------------------------------------------------------*
001800110308      * ? C O S T A N T I ?
001900110308     d c_ColorGrn      c                   const(x'20')
002000110308     d c_ColorWht      c                   const(x'22')
002100110308     d c_ColorTrq      c                   const(x'30')
002200110308     d c_ColorBlu      c                   const(x'3A')
002300110308
002400070216      * ? V A R I A B I L I ?
002500080327     d dataiso         s               d
002600080327     d dataeur         s               d   datfmt(*eur)
002700080327     d nrrplg          s              9  0
002800080327     d esitol          s              1
002900110307       // -?Nome specifico libreria/file del file di filiale?
003000110307     d wLibFIDST       s             21a   inz
003100070504
003200110307      * ? D S   I N T E R N E / E S T E R N E ?
003300070504     d wdatainv        ds
003400070508     d  waai                   1      4  0
003500070508     d  wmmi                   5      6  0
003600070508     d  wggi                   7      8  0
003700070511
003800080327     d azuteds       e ds                  extname(azute00f)
003900070209     d ddatiute      e ds
004000070508     d dprc          e ds
004100080327     d kpjba         e ds
004200070504     d fiplgds       e ds                  extname(fiplg00f)
004300070209     d fnlv24ds      e ds
004400070508     d tibs02ds      e ds
004500070209     d tibs34ds      e ds                  inz
004600110307
004700110307      * ? P R O C E D U R E   U S A T E ?
004800110307       // -?Reperimento NETA sistema AS/400 corrente?
004900110307     d currSysNeta     s              8a   inz
005000110307      /copy gaitrasrc/srcProtoPr,UBRTVNETA
005100080327      *------------------------------------------------------------------------*
005200080327     c     nrrplg        chain     fiplg00f
005300080327     c                   if        not %found(fiplg00f)
005400080327     c                   eval      esitol ='E'
005500080327     c                   exsr      srfine
005600080327     c                   end
005700080327      *
005800080327     c                   eval      v3ctiporcd = PLGTIPORCD
005900070514     c                   clear                   v3dtiporcd
006000070508     c                   clear                   tibs02ds
006100070508     c                   eval      t02mod = 'C'
006200070508     c                   eval      t02sif = knsif
006300070508     c                   eval      t02cod = 'PRC'
006400070514     c                   eval      t02ke1 = v3ctiporcd
006500070508     c                   call      'TIBS02R'
006600070508     c                   parm                    kpjba
006700070508     c                   parm                    tibs02ds
006800070508     c                   if        t02err = *blanks
006900070508     c                   eval      dprc = t02uni
007000070508     c                   endif
007100080327     c                   eval      vtcpgm = D§PRCPGML
007200080327     c                   if        D§PRCPGML = ' '
007300080327     c                   eval      esitol ='E'
007400080327     c                   exsr      srfine
007500080327     c                   end
007600080327     c                   eval      v3dtiporcd = d§prcdes
007700070508      * tipo applicazione
007800080327     c                   eval      v3ctipappl = PLGTIPAPPL
007900070514     c                   clear                   v3dtipappl
008000070514     c                   if        v3ctipappl = 'C'
008100070514     c                   eval      v3dtipappl = 'Consegna'
008200070508     c                   endif
008300070508      * profilo
008400080327     c                   eval      v3cprfc = PLGPRFC
008500070508      * id dispositivo
008600080327     c                   eval      v3ciddisp = PLGIDDISP
008700070508      * data/ora ricezione
008800080327     c                   eval      wdatainv = %subst(plgdatora:1:8)
008900080327     c                   movel     wdatainv      v3cdata
009000080409     c                   move      v3cdata       dataiso
009100080409     c                   move      dataiso       dataeur
009200080409     c                   move      dataeur       v3cdata
009300080327     c                   move      PLGDATORA     v3cora
009400070508      * id documento
009500080327     c                   eval      v3ciddoc = PLGIDDOC
009600070508      * autotrasportatore
009700080327     c                   eval      v3ccodaut = PLGCODAUT
009800070508     c                   clear                   v3dcodaut
009900070508     c                   eval      apdtip = 'A'
010000070508     c                   eval      apdpdr = v3ccodaut
010100070508     c     kfiapd        chain     fiapd01l
010200070508     c                   if        %found(fiapd01l) and apdatb = *blanks
010300070508     c                   eval      v3dcodaut = apdrsc
010400070508     c                   endif
010500070508      * filiale distinta
010600080327     c                   eval      v3cfgs = PLGfgs
010700070508     c                   clear                   v3dfgs
010800070508     c     v3cfgs        chain     azorg01L
010900070508     c                   if        %found(azorg01l) and orgfva = *blanks
011000070508     c                   eval      v3dfgs = orgdes
011100070508     c                   endif
011200070508      * numero distinta
011300080327     c                   eval      v3cndc = PLGNDC
011400070508     c                   clear                   v3dndc
011500070508     c                   eval      dstnpg = 4
011600070508     c                   eval      dstfgs = v3cfgs
011700070508     c                   eval      dstnfv = v3cndc
011800070508     c     kfidst        chain     fidst01l
011900070508     c                   if        %found(fidst01l)
012000070508     c                   eval      apdtip = 'A'
012100070508     c                   eval      apdpdr = dstpdr
012200070508     c                   clear                   apdrsc
012300070508     c     kfiapd        chain     fiapd01l
012400070508     c                   if        %found(fiapd01l)
012500110308     c                   eval      v3dndc = 'Del' +
012600110308     c                             c_ColorTrq +
012700070508     c                             %subst(%editc(%dec(dstdfv):'X'):7:2) + '/' +
012800070508     c                             %subst(%editc(%dec(dstdfv):'X'):5:2) + '/' +
012900070508     c                             %subst(%editc(%dec(dstdfv):'X'):1:4) + ' ' +
013000110308     c                             %editc(%dec(dstpdr):'X') +
013100110308     c                             c_ColorBlu +
013200070508     c                             %subst(apdrsc:1:17)
013300070508     c                   endif
013400070508     c                   endif
013500070508      * messaggio errore
013600080327     c                   eval      v3cmsg = PLGMSG
013700070508
013800080327     c                   write     lvp3t01
013900080327     c                   write     lvp3d01
014000080327     c* chiamo il pgm per tipo record x emettere il video
014100110322     c                   movel(p)  V3Ctipappl    kpjbu
014200080327     c                   call      D§PRCPGML
014300080328     c                   parm                    kpjba
014400080328     c                   parm                    plgdati
014500080327     c                   parm                    esitol
014600080327      * f3=fine, f12=guida
014700080327     c                   if        esitol <> ' '
014800080327     c                   exsr      srfine
014900070508     c                   endif
015000070510
015100070510      *------------------------------------------------------------------------*
015200070510      *? PULIZIA DELLA VIDEATA  ?
015300070510      *------------------------------------------------------------------------*
015400070510     c     sr_pulvid03   begsr
015500070510
015600070514     c                   clear                   v3ctiporcd
015700070514     c                   clear                   v3dtiporcd
015800070514     c                   clear                   v3ctipappl
015900070514     c                   clear                   v3dtipappl
016000070510     c                   clear                   v3cprfc
016100070510     c                   clear                   v3ciddisp
016200070510     c                   clear                   v3cdata
016300070510     c                   clear                   v3cora
016400070510     c                   clear                   v3ciddoc
016500070510     c                   clear                   v3ccodaut
016600070510     c                   clear                   v3dcodaut
016700070510     c                   clear                   v3cfgs
016800070510     c                   clear                   v3dfgs
016900070510     c                   clear                   v3cndc
017000070510     c                   clear                   v3dndc
017100070510     c                   clear                   v3cmsg
017200070510
017300070510     c                   endsr
017400070504      *------------------------------------------------------------------------*
017500070504      *? ROUTINE INIZIALE
017600070504      *------------------------------------------------------------------------*
017700080327     c     srfine        begsr
017800080327     c                   seton                                        lr
017900080327     c                   return
018000080327     c                   endsr
018100080327      *------------------------------------------------------------------------*
018200080327      *? ROUTINE INIZIALE
018300080327      *------------------------------------------------------------------------*
018400080327     c     *inzsr        begsr
018500080327
018600070504     c     *entry        plist
018700080327     c                   parm                    kpjba
018800080327     c                   parm                    nrrplg
018900080327     c                   parm                    esitol
019000080327     c                   clear                   esitol
019100070504
019200070504     c     *dtaara       define    §azute        azuteds
019300070504     c     *dtaara       define    §datiute      ddatiute
019400070504     c                   in(e)     *dtaara
019500070504     c                   if        %error  or rsut = *blanks
019600070504     c                   clear                   tibs34ds
019700070504     c                   call      'TIBS34R'
019800070504     c                   parm                    tibs34ds
019900070504     c                   in        *dtaara
020000070504     c                   endif
020100110307      * -?Verifica del sistema AS/400 corrente?
020200110307     c                   if        UBRTVNETA_Rtv(currSysNeta) <> *zero
020300110307     c                   eval      *inlr = *on
020400110307     c                   return
020500110307     c                   endif
020600110307      * -?Impostazione libreria archivi di filiale?
020700110307     c                   if        %subst ( currSysNeta : 1 : 6 ) = 'SETRAS'
020800110307     c                   eval      wLibFIDST = 'FILTRA201/FIDST01L'
020900110307     c                   else
021000110307     c                   eval      wLibFIDST = 'FILTRAPRD/FIDST01L'
021100110307     c                   endif
021200110307      * -?Apertura archivi di filiale?
021300110307     c                   open      FIDST01L
021400070504
021500070504     c     kfidst        klist
021600070504     c                   kfld                    dstnpg
021700070504     c                   kfld                    dstnfv
021800070508     c                   kfld                    dstfgs
021900070504
022000070209     c     kfiapd        klist
022100070214     c                   kfld                    apdtip
022200070214     c                   kfld                    apdpdr
022300070504
022400070504     c                   endsr
