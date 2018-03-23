000100070209     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200080328     h dftactgrp(*no) actgrp(*caller)
000300110307     h BndDir('UBBNDDIR')
000400070209      *------------------------------------------------------------------------*
000500070209      *
000600080327      * Interrogazione Log pda consegne
000700070209      *
000800070209      *------------------------------------------------------------------------*
000900080327     fazorg01l  if   e           k disk
001000080327     ffiapd01l  if   e           k disk
001100110307     ffidst01l  if   e           k disk    extfile(wLibFIDST)  usropn
001200080327     ffiplg00f  if   e             disk
001300080327     ffnlvp3d   cf   e             workstn
001400070209      *------------------------------------------------------------------------*
001500110308      * ? C O S T A N T I ?
001600110308     d c_ColorGrn      c                   const(x'20')
001700110308     d c_ColorWht      c                   const(x'22')
001800110308     d c_ColorTrq      c                   const(x'30')
001900110308     d c_ColorBlu      c                   const(x'3A')
002000110308
002100070216      * ? V A R I A B I L I ?
002200080327     d dataiso         s               d
002300080327     d dataeur         s               d   datfmt(*eur)
002400080327     d nrrplg          s              9  0
002500080327     d esitol          s              1
002600110307       // -?Nome specifico libreria/file del file di filiale?
002700110307     d wLibFIDST       s             21a   inz
002800070504
002900110307      * ? D S   I N T E R N E / E S T E R N E ?
003000070504     d wdatainv        ds
003100070508     d  waai                   1      4  0
003200070508     d  wmmi                   5      6  0
003300070508     d  wggi                   7      8  0
003400070511
003500080327     d azuteds       e ds                  extname(azute00f)
003600070209     d ddatiute      e ds
003700070508     d dprc          e ds
003800080327     d kpjba         e ds
003900070504     d fiplgds       e ds                  extname(fiplg00f)
004000070209     d fnlv24ds      e ds
004100070508     d tibs02ds      e ds
004200070209     d tibs34ds      e ds                  inz
004300110307
004400110307      * ? P R O C E D U R E   U S A T E ?
004500110307       // -?Reperimento NETA sistema AS/400 corrente?
004600110307     d currSysNeta     s              8a   inz
004700110307      /copy gaitrasrc/srcProtoPr,UBRTVNETA
004800080327      *------------------------------------------------------------------------*
004900080327     c     nrrplg        chain     fiplg00f
005000080327     c                   if        not %found(fiplg00f)
005100080327     c                   eval      esitol ='E'
005200080327     c                   exsr      srfine
005300080327     c                   end
005400080327      *
005500080327     c                   eval      v3ctiporcd = PLGTIPORCD
005600070514     c                   clear                   v3dtiporcd
005700070508     c                   clear                   tibs02ds
005800070508     c                   eval      t02mod = 'C'
005900070508     c                   eval      t02sif = knsif
006000070508     c                   eval      t02cod = 'PRC'
006100070514     c                   eval      t02ke1 = v3ctiporcd
006200070508     c                   call      'TIBS02R'
006300070508     c                   parm                    kpjba
006400070508     c                   parm                    tibs02ds
006500070508     c                   if        t02err = *blanks
006600070508     c                   eval      dprc = t02uni
006700070508     c                   endif
006800080327     c                   eval      vtcpgm = D§PRCPGML
006900080327     c                   if        D§PRCPGML = ' '
007000080327     c                   eval      esitol ='E'
007100080327     c                   exsr      srfine
007200080327     c                   end
007300080327     c                   eval      v3dtiporcd = d§prcdes
007400070508      * tipo applicazione
007500080327     c                   eval      v3ctipappl = PLGTIPAPPL
007600070514     c                   clear                   v3dtipappl
007700070514     c                   if        v3ctipappl = 'C'
007800070514     c                   eval      v3dtipappl = 'Consegna'
007900070508     c                   endif
008000070508      * profilo
008100080327     c                   eval      v3cprfc = PLGPRFC
008200070508      * id dispositivo
008300080327     c                   eval      v3ciddisp = PLGIDDISP
008400070508      * data/ora ricezione
008500080327     c                   eval      wdatainv = %subst(plgdatora:1:8)
008600080327     c                   movel     wdatainv      v3cdata
008700080409     c                   move      v3cdata       dataiso
008800080409     c                   move      dataiso       dataeur
008900080409     c                   move      dataeur       v3cdata
009000080327     c                   move      PLGDATORA     v3cora
009100070508      * id documento
009200080327     c                   eval      v3ciddoc = PLGIDDOC
009300070508      * autotrasportatore
009400080327     c                   eval      v3ccodaut = PLGCODAUT
009500070508     c                   clear                   v3dcodaut
009600070508     c                   eval      apdtip = 'A'
009700070508     c                   eval      apdpdr = v3ccodaut
009800070508     c     kfiapd        chain     fiapd01l
009900070508     c                   if        %found(fiapd01l) and apdatb = *blanks
010000070508     c                   eval      v3dcodaut = apdrsc
010100070508     c                   endif
010200070508      * filiale distinta
010300080327     c                   eval      v3cfgs = PLGfgs
010400070508     c                   clear                   v3dfgs
010500070508     c     v3cfgs        chain     azorg01L
010600070508     c                   if        %found(azorg01l) and orgfva = *blanks
010700070508     c                   eval      v3dfgs = orgdes
010800070508     c                   endif
010900070508      * numero distinta
011000080327     c                   eval      v3cndc = PLGNDC
011100070508     c                   clear                   v3dndc
011200070508     c                   eval      dstnpg = 4
011300070508     c                   eval      dstfgs = v3cfgs
011400070508     c                   eval      dstnfv = v3cndc
011500070508     c     kfidst        chain     fidst01l
011600070508     c                   if        %found(fidst01l)
011700070508     c                   eval      apdtip = 'A'
011800070508     c                   eval      apdpdr = dstpdr
011900070508     c                   clear                   apdrsc
012000070508     c     kfiapd        chain     fiapd01l
012100070508     c                   if        %found(fiapd01l)
012200110308     c                   eval      v3dndc = 'Del' +
012300110308     c                             c_ColorTrq +
012400070508     c                             %subst(%editc(%dec(dstdfv):'X'):7:2) + '/' +
012500070508     c                             %subst(%editc(%dec(dstdfv):'X'):5:2) + '/' +
012600070508     c                             %subst(%editc(%dec(dstdfv):'X'):1:4) + ' ' +
012700110308     c                             %editc(%dec(dstpdr):'X') +
012800110308     c                             c_ColorBlu +
012900070508     c                             %subst(apdrsc:1:17)
013000070508     c                   endif
013100070508     c                   endif
013200070508      * messaggio errore
013300080327     c                   eval      v3cmsg = PLGMSG
013400070508
013500080327     c                   write     lvp3t01
013600080327     c                   write     lvp3d01
013700080327     c* chiamo il pgm per tipo record x emettere il video
013800080327     c                   call      D§PRCPGML
013900080328     c                   parm                    kpjba
014000080328     c                   parm                    plgdati
014100080327     c                   parm                    esitol
014200080327      * f3=fine, f12=guida
014300080327     c                   if        esitol <> ' '
014400080327     c                   exsr      srfine
014500070508     c                   endif
014600070510
014700070510      *------------------------------------------------------------------------*
014800070510      *? PULIZIA DELLA VIDEATA  ?
014900070510      *------------------------------------------------------------------------*
015000070510     c     sr_pulvid03   begsr
015100070510
015200070514     c                   clear                   v3ctiporcd
015300070514     c                   clear                   v3dtiporcd
015400070514     c                   clear                   v3ctipappl
015500070514     c                   clear                   v3dtipappl
015600070510     c                   clear                   v3cprfc
015700070510     c                   clear                   v3ciddisp
015800070510     c                   clear                   v3cdata
015900070510     c                   clear                   v3cora
016000070510     c                   clear                   v3ciddoc
016100070510     c                   clear                   v3ccodaut
016200070510     c                   clear                   v3dcodaut
016300070510     c                   clear                   v3cfgs
016400070510     c                   clear                   v3dfgs
016500070510     c                   clear                   v3cndc
016600070510     c                   clear                   v3dndc
016700070510     c                   clear                   v3cmsg
016800070510
016900070510     c                   endsr
017000070504      *------------------------------------------------------------------------*
017100070504      *? ROUTINE INIZIALE
017200070504      *------------------------------------------------------------------------*
017300080327     c     srfine        begsr
017400080327     c                   seton                                        lr
017500080327     c                   return
017600080327     c                   endsr
017700080327      *------------------------------------------------------------------------*
017800080327      *? ROUTINE INIZIALE
017900080327      *------------------------------------------------------------------------*
018000080327     c     *inzsr        begsr
018100080327
018200070504     c     *entry        plist
018300080327     c                   parm                    kpjba
018400080327     c                   parm                    nrrplg
018500080327     c                   parm                    esitol
018600080327     c                   clear                   esitol
018700070504
018800070504     c     *dtaara       define    §azute        azuteds
018900070504     c     *dtaara       define    §datiute      ddatiute
019000070504     c                   in(e)     *dtaara
019100070504     c                   if        %error  or rsut = *blanks
019200070504     c                   clear                   tibs34ds
019300070504     c                   call      'TIBS34R'
019400070504     c                   parm                    tibs34ds
019500070504     c                   in        *dtaara
019600070504     c                   endif
019700110307      * -?Verifica del sistema AS/400 corrente?
019800110307     c                   if        UBRTVNETA_Rtv(currSysNeta) <> *zero
019900110307     c                   eval      *inlr = *on
020000110307     c                   return
020100110307     c                   endif
020200110307      * -?Impostazione libreria archivi di filiale?
020300110307     c                   if        %subst ( currSysNeta : 1 : 6 ) = 'SETRAS'
020400110307     c                   eval      wLibFIDST = 'FILTRA201/FIDST01L'
020500110307     c                   else
020600110307     c                   eval      wLibFIDST = 'FILTRAPRD/FIDST01L'
020700110307     c                   endif
020800110307      * -?Apertura archivi di filiale?
020900110307     c                   open      FIDST01L
021000070504
021100070504     c     kfidst        klist
021200070504     c                   kfld                    dstnpg
021300070504     c                   kfld                    dstnfv
021400070508     c                   kfld                    dstfgs
021500070504
021600070209     c     kfiapd        klist
021700070214     c                   kfld                    apdtip
021800070214     c                   kfld                    apdpdr
021900070504
022000070504     c                   endsr
