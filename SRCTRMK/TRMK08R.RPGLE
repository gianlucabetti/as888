000100020925     h Decedit('0,') Datedit(*dmy.) option(*nodebugio)
000200050119      *------------------------------------------------------------------------*
000300081215     fTncpo01l  if   e           K Disk    infds(cpo01)
000400081215     f                                     rename(tncpo000:tncpo001)
000500081215     fTncpo00f  uf   e             Disk
000600090520     fTicpi01l  if   e           K Disk
000700090520     fazorg01l  if   e           K Disk
000800090521     FWFDEmr1l  iF   E           K DISK    prefix(r_)  usropn
000900090521     f                                     extfile(Wfdemr)
001000090521     fQsysprt   o    f  132        printer usropn
001100050119
001200050119      *------------------------------------------------------------------------*
001300050119      *   C A M P I   D I   L A V O R O
001400050119      *------------------------------------------------------------------------*
001500081215     D CPO01           DS
001600081215     D  cp1NRR               397    400B 0
001700081215
001800081215     d KPJBA         e ds
001900090520     d TRMK50ds      e ds                  inz
002000100302     d TRMK52ds      e ds                  inz
002100081215     d dcpo01        e ds                  inz
002200090520     d fnlv13ds      e ds
002300090520     d tisi95ds      e ds
002400090520     d
002500090520     d ultdata         s              8  0
002600090520     d demare          s              3  0
002700100302     d infoATR         s                    like(cpival)
002800090520     d wcpo            s                    like(cpocpo)
002900090518     d PiccoleAL       s             11  0  inz(10000000)
003000090518     d MedieAL         s             11  0  inz(50000000)
003100090518     d Perc            s              5  2
003200090518     d w0080           s              8  0
003300090518     d alfasct         s              5
003400090518     d spesatra        s             11  0
003500090521     d demtotfat       s             15  0
003600090520     d demfil          s              3  0
003700090520     d Datasys         s               d   inz(*sys) datfmt(*iso)
003800090521     d WFDEMR          s             21    inz('GAITRAAZM /WFDEMR1L')
003900090520      *------------------------------------------------------------------------*
004000090520     D FNLV13R         pr                  extpgm('FNLV13R')
004100090520     D  kpjba                              likeds(kpjba)
004200090520     D  FNLV13DS                           likeds(FNLV13DS)
004300090520     D  TISI95DS                           likeds(TISI95DS)
004400090520     d
004500090520      /copy gaitrasrc/srcprotopr,trmk50r
004600100302      /copy gaitrasrc/srcprotopr,trmk52r
004700050119      *------------------------------------------------------------------------*
004800090520     c     *entry        plist
004900090520     c                   parm                    kpjba
005000090520     c                   parm                    paramcpo         11
005100090520     c                   if        %parms=2 and paramcpo>*zeros
005200090520     c                   eval      wcpo=%int(paramcpo)
005300090520     c                   else
005400090520     c                   clear                   wcpo
005500090520     c                   endif
005600090521     c
005700090521     C                   OPEN(e)   wfdemr1l
005800090521     c
005900090521     c                   if        not %open(wfdemr1l)
006000090521     c                   eval      %subst(WFDEMR:7:4)='AZP '
006100090521     C                   OPEN      wfdemr1l
006200090521     c                   endif
006300080227      /free
006400090520       if wcpo>0    ;
006500090520       chain wcpo    tncpo01l ;
006600090520       if %found(tncpo01l)   ;
006700090520       exsr elabora    ;
006800090520       endif         ;
006900090520
007000090520       else   ;
007100090521       open qsysprt   ;
007200090521
007300090520       setll *loval  tncpo01l   ;
007400081215       read          tncpo01l   ;
007500080227
007600081215       dow  not %eof(tncpo01l)  ;
007700081215
007800090520       exsr   Elabora   ;
007900081215
008000081215       read          tncpo01l   ;
008100080227       enddo   ;
008200090520       endif   ;
008300090520
008400080227       Eval      *InLr = *On         ;
008500090520       //---------------------------------------------
008600090520       BEGSR  Elabora  ;
008700090520
008800090520       // Aggiorno data ultimo aggiornamento info
008900090520       //  e spesa trasporti se reale
009000090520
009100100302         clear infoATR  ;
009200090520         clear ultdata  ;
009300090520         clear spesatra ;
009400090520         dcpo01=cporst   ;
009500090520         setll  cpocpo  ticpi01l ;
009600090520         reade  cpocpo  ticpi01l ;
009700090520
009800090520         dow not %eof(ticpi01l)    ;
009900090520         if cpidim>ultdata ;
010000090520         ultdata=cpidim  ;
010100090520         endif  ;
010200090520         if cpitpf='SPT'   ;
010300090520         spesatra=cpipft   ;
010400090520         endif  ;
010500100302         if cpitpf='ATR'   ;
010600100302         infoATR =cpival   ;
010700100302         endif  ;
010800100302
010900090520
011000090520         reade  cpocpo  ticpi01l ;
011100090520         enddo ;
011200090520
011300090520         exsr Aggio    ;
011400090520         ENDSR  ;
011500081215       //---------------------------------------------
011600081215                    BEGSr Aggio   ;
011700100302                      clear trmk52ds   ;
011800100302
011900090521                       chain(e)  cp1nrr    tncpo00f   ;
012000090521
012100090521                       if %error             ;
012200090521                       if wcpo=0    ;
012300090521                       except ERRORE   ;
012400090521                       endif    ;
012500090521
012600090521                       else ;
012700081215                      dcpo01=cporst    ;
012800090520
012900090520                      // Ultima data info comm
013000090520                      §cpoifodul=%editc(ultdata:'X')     ;
013100090520
013200090520                      // Spesa trasporti presunta/totale
013300090520                      if  spesatra=0         ;
013400090518                      exsr   calcol   ;
013500100302
013600100302                      i52spt=spesatra   ;
013700100302                      i52tspt='P'  ;
013800100302                      else   ;
013900100302                      i52spt=spesatra   ;
014000090518                      endif   ;
014100100302
014200100302                      i52atr=infoATR   ;
014300100302                      i52cpo=cpocpo    ;
014400100302
014500100302                      // flag importanza cliete potenziale
014600100302                      callp TRMK52R (trmk52ds)  ;
014700100302                      cpoftr=o52cimp        ;
014800100302
014900100302                      // Spesa trasporti
015000090518                      §cposptp =spesatra    ;
015100090520
015200090520                      // Info totali
015300090520                    clear trmk50ds    ;
015400090520                    i50cpo=cpocpo     ;
015500090520                    i50mod='C'        ;
015600090520                    i50obl='S'        ;
015700090520                    callp TRMK50R (kpjba:trmk50ds)  ;
015800090520                      §cpoifotot=o50ifotot   ;
015900090520
016000081215                      cporst=dcpo01    ;
016100081215                      update tncpo000  ;
016200090521                      endif  ;
016300090521
016400081215                      ENDSR   ;
016500090518       //--------------------------------------------------------------
016600090520       BEGSR Calcol   ;
016700090520 1     if  cpoftaz  >0 ;
016800090521       demtotfat = cpoftaz*1000   ;
016900090520
017000090520       clear demare   ;
017100090520
017200090520       // se presente la nazione, imposto la filiale del codice
017300090520       //  e non da cappario
017400090520 2     if CPOnaz<> '  '   ;
017500090520       DEMFIL=CPOflt    ;
017600090520 x2    ELSE   ;
017700090520
017800090520       clear   tisi95ds  ;
017900090520       clear   fnlv13ds  ;
018000090520       i95tcn='7'        ;
018100090520       i13la3='S'     ;
018200090520       i95cap=cpocap  ;
018300090520       i95loc=cpocit  ;
018400090520       i95prv=cpoprv  ;
018500090520       i95nar=cponaz  ;
018600090520       // data di elaborazione: 1 gennaio anno in corso
018700090520       i95dat=%dec(datasys )                         ;
018800090520       i13af0='S'     ;
018900090520       i13cnv=' '     ;
019000090520       i13af1=' '     ;
019100090520 3     if %subst(i95cap:1:2)='47 'and  i95prv='  ' ;
019200090520            I95PRV='RN'  ;
0193000905203        endif  ;
019400090520
019500090520       callp   FNLV13R  (kpjba:fnlv13ds:tisi95ds) ;
019600090520
019700090520       demfil=o95lna       ;
0198000905202      endif   ;
019900090520
020000090520       // se trovato imposto area
020100090520
0202000905202      if    demfil>0    ;
020300090520       chain   demfil   AZORG01L   ;
0204000905203      if %found(azorg01l) ;
020500090520         demare=orgcar ;
0206000905203      endif  ;
0207000905202      endif  ;
020800090520
020900090520
021000090520         alfasct=%editc(cposct:'X')     ;
021100090520         clear perc ;
021200090520
021300090520       // Ricavo l'area di appartenenza dall'indirizzo
021400090518
021500090520         chain (alfasct :demare)   wfdemr1l   ;
021600090518
021700090521       if %found(wfdemr1l)  ;
021800090520         exsr ScegliP    ;
021900090521       endif ;
022000090518
022100090518       if perc=0   ;
022200090520       clear demare    ;
022300090518       chain (alfasct : 000)   wfdemr1l   ;
022400090518
022500090521       if %found(wfdemr1l) ;
022600090518       exsr ScegliP    ;
022700090520       endif   ;
022800090521       endif   ;
022900090518
023000090518       if perc>0   ;
023100090521       eval(H) spesatra =(demtotfat *perc)/100   ;
023200090518
023300090518       // Arrotondo alle 1000 superiori
023400090518       w0080=spesatra /1000 ;
023500090518       w0080=w0080+1  ;
023600090518       spesatra =w0080*1000  ;
023700090518       endif   ;
023800090520       endif   ;
023900090518
024000090518       ENDSR   ;
024100090518       //--------------------------------------------------------------
024200090518       BEGSR ScegliP   ;
024300090518       select   ;
024400090521       when demtotfat<=piccoleAL  ;
024500090520       if r_dempcli>9  or demare=0  ;
024600090518       perc=r_dempperc  ;
024700090518       endif   ;
024800090521       when demtotfat<=MedieAL  ;
024900090520       if r_demmcli>9   or demare=0 ;
025000090518       perc=r_demmperc  ;
025100090518       endif   ;
025200090518       other  ;
025300090520       if r_demgcli>9   or demare=0 ;
025400090518       perc=r_demgperc  ;
025500090520       endif   ;
025600090518       endsl   ;
025700090518       ENDSR   ;
025800090518
025900080227
026000080227      /end-free
026100090521     oQsysprt   e            ERRORE      1
026200090521     o                                              'Potenziale non aggiornabil'
026300090521     o                                        +   0 'e perchè allocato'
026400090521     o                       cpOCPO           +   2
