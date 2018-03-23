000100020925     h Decedit('0,') Datedit(*dmy.) option(*nodebugio)
000200050119      *------------------------------------------------------------------------*
000300081215     fTncpo01l  if   e           K Disk    infds(cpo01)
000400081215     f                                     rename(tncpo000:tncpo001)
000500081215     fTncpo00f  uf   e             Disk
000600090520     fTicpi01l  if   e           K Disk
000700090520     fazorg01l  if   e           K Disk
000800090518     FWFDEmr1l  iF   E           K DISK    prefix(r_)
000900050119     fQsysprt   o    f  132        printer
001000050119
001100050119      *------------------------------------------------------------------------*
001200050119      *   C A M P I   D I   L A V O R O
001300050119      *------------------------------------------------------------------------*
001400081215     D CPO01           DS
001500081215     D  cp1NRR               397    400B 0
001600081215
001700081215     d KPJBA         e ds
001800090520     d TRMK50ds      e ds                  inz
001900081215     d dcpo01        e ds                  inz
002000090520     d fnlv13ds      e ds
002100090520     d tisi95ds      e ds
002200090520     d
002300090520     d ultdata         s              8  0
002400090520     d demare          s              3  0
002500090520     d wcpo            s                    like(cpocpo)
002600090518     d PiccoleAL       s             11  0  inz(10000000)
002700090518     d MedieAL         s             11  0  inz(50000000)
002800090518     d Perc            s              5  2
002900090518     d w0080           s              8  0
003000090518     d alfasct         s              5
003100090518     d spesatra        s             11  0
003200090520     d demfil          s              3  0
003300090520     d Datasys         s               d   inz(*sys) datfmt(*iso)
003400090520      *------------------------------------------------------------------------*
003500090520     D FNLV13R         pr                  extpgm('FNLV13R')
003600090520     D  kpjba                              likeds(kpjba)
003700090520     D  FNLV13DS                           likeds(FNLV13DS)
003800090520     D  TISI95DS                           likeds(TISI95DS)
003900090520     d
004000090520      /copy gaitrasrc/srcprotopr,trmk50r
004100050119      *------------------------------------------------------------------------*
004200090520     c     *entry        plist
004300090520     c                   parm                    kpjba
004400090520     c                   parm                    paramcpo         11
004500090520     c                   if        %parms=2 and paramcpo>*zeros
004600090520     c                   eval      wcpo=%int(paramcpo)
004700090520     c                   else
004800090520     c                   clear                   wcpo
004900090520     c                   endif
005000080227      /free
005100090520       if wcpo>0    ;
005200090520       chain wcpo    tncpo01l ;
005300090520       if %found(tncpo01l)   ;
005400090520       exsr elabora    ;
005500090520       endif         ;
005600090520
005700090520       else   ;
005800090520       setll *loval  tncpo01l   ;
005900081215       read          tncpo01l   ;
006000080227
006100081215       dow  not %eof(tncpo01l)  ;
006200081215
006300090520       exsr   Elabora   ;
006400081215
006500081215       read          tncpo01l   ;
006600080227       enddo   ;
006700090520       endif   ;
006800090520
006900080227       Eval      *InLr = *On         ;
007000090520       //---------------------------------------------
007100090520       BEGSR  Elabora  ;
007200090520
007300090520       // Aggiorno data ultimo aggiornamento info
007400090520       //  e spesa trasporti se reale
007500090520
007600090520         clear ultdata  ;
007700090520         clear spesatra ;
007800090520         dcpo01=cporst   ;
007900090520         setll  cpocpo  ticpi01l ;
008000090520         reade  cpocpo  ticpi01l ;
008100090520
008200090520         dow not %eof(ticpi01l)    ;
008300090520         if cpidim>ultdata ;
008400090520         ultdata=cpidim  ;
008500090520         endif  ;
008600090520         if cpitpf='SPT'   ;
008700090520         spesatra=cpipft   ;
008800090520         endif  ;
008900090520
009000090520         reade  cpocpo  ticpi01l ;
009100090520         enddo ;
009200090520
009300090520         exsr Aggio    ;
009400090520         ENDSR  ;
009500081215       //---------------------------------------------
009600081215                    BEGSr Aggio   ;
009700081215                       chain  cp1nrr    tncpo00f   ;
009800081215                      dcpo01=cporst    ;
009900090520
010000090520                      // Ultima data info comm
010100090520                      §cpoifodul=%editc(ultdata:'X')     ;
010200090520
010300090520                      // Spesa trasporti presunta/totale
010400090520                      if  spesatra=0         ;
010500090518                      exsr   calcol   ;
010600090518                      endif   ;
010700090518                      §cposptp =spesatra    ;
010800090520
010900090520                      // Info totali
011000090520                    clear trmk50ds    ;
011100090520                    i50cpo=cpocpo     ;
011200090520                    i50mod='C'        ;
011300090520                    i50obl='S'        ;
011400090520                    callp TRMK50R (kpjba:trmk50ds)  ;
011500090520                      §cpoifotot=o50ifotot   ;
011600090520
011700081215                      cporst=dcpo01    ;
011800081215                      update tncpo000  ;
011900081215                      ENDSR   ;
012000090518       //--------------------------------------------------------------
012100090520       BEGSR Calcol   ;
012200090520 1     if  cpoftaz  >0 ;
012300090520
012400090520       clear demare   ;
012500090520
012600090520       // se presente la nazione, imposto la filiale del codice
012700090520       //  e non da cappario
012800090520 2     if CPOnaz<> '  '   ;
012900090520       DEMFIL=CPOflt    ;
013000090520 x2    ELSE   ;
013100090520
013200090520       clear   tisi95ds  ;
013300090520       clear   fnlv13ds  ;
013400090520       i95tcn='7'        ;
013500090520       i13la3='S'     ;
013600090520       i95cap=cpocap  ;
013700090520       i95loc=cpocit  ;
013800090520       i95prv=cpoprv  ;
013900090520       i95nar=cponaz  ;
014000090520       // data di elaborazione: 1 gennaio anno in corso
014100090520       i95dat=%dec(datasys )                         ;
014200090520       i13af0='S'     ;
014300090520       i13cnv=' '     ;
014400090520       i13af1=' '     ;
014500090520 3     if %subst(i95cap:1:2)='47 'and  i95prv='  ' ;
014600090520            I95PRV='RN'  ;
0147000905203        endif  ;
014800090520
014900090520       callp   FNLV13R  (kpjba:fnlv13ds:tisi95ds) ;
015000090520
015100090520       demfil=o95lna       ;
0152000905202      endif   ;
015300090520
015400090520       // se trovato imposto area
015500090520
0156000905202      if    demfil>0    ;
015700090520       chain   demfil   AZORG01L   ;
0158000905203      if %found(azorg01l) ;
015900090520         demare=orgcar ;
0160000905203      endif  ;
0161000905202      endif  ;
016200090520
016300090520
016400090520         alfasct=%editc(cposct:'X')     ;
016500090520         clear perc ;
016600090520
016700090520       // Ricavo l'area di appartenenza dall'indirizzo
016800090518
016900090520         chain (alfasct :demare)   wfdemr1l   ;
017000090518
017100090520         exsr ScegliP    ;
017200090518
017300090518       if perc=0   ;
017400090520       clear demare    ;
017500090518       chain (alfasct : 000)   wfdemr1l   ;
017600090518
017700090518       exsr ScegliP    ;
017800090520       endif   ;
017900090518
018000090518       if perc>0   ;
018100090518       eval(H) spesatra =(cpoftaz  *perc)/100   ;
018200090518
018300090518       // Arrotondo alle 1000 superiori
018400090518       w0080=spesatra /1000 ;
018500090518       w0080=w0080+1  ;
018600090518       spesatra =w0080*1000  ;
018700090518       endif   ;
018800090520       endif   ;
018900090518
019000090518       ENDSR   ;
019100090518       //--------------------------------------------------------------
019200090518       BEGSR ScegliP   ;
019300090518       select   ;
019400090518       when cpoftaz  <=piccoleAL  ;
019500090520       if r_dempcli>9  or demare=0  ;
019600090518       perc=r_dempperc  ;
019700090518       endif   ;
019800090518       when cpoftaz  <=MedieAL  ;
019900090520       if r_demmcli>9   or demare=0 ;
020000090518       perc=r_demmperc  ;
020100090518       endif   ;
020200090518       other  ;
020300090520       if r_demgcli>9   or demare=0 ;
020400090518       perc=r_demgperc  ;
020500090520       endif   ;
020600090518       endsl   ;
020700090518       ENDSR   ;
020800090518
020900080227
021000080227      /end-free
