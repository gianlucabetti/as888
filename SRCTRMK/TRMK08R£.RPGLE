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
002000081215     d dcpo01        e ds                  inz
002100090520     d fnlv13ds      e ds
002200090520     d tisi95ds      e ds
002300090520     d
002400090520     d ultdata         s              8  0
002500090520     d demare          s              3  0
002600090520     d wcpo            s                    like(cpocpo)
002700090518     d PiccoleAL       s             11  0  inz(10000000)
002800090518     d MedieAL         s             11  0  inz(50000000)
002900090518     d Perc            s              5  2
003000090518     d w0080           s              8  0
003100090518     d alfasct         s              5
003200090518     d spesatra        s             11  0
003300090521     d demtotfat       s             15  0
003400090520     d demfil          s              3  0
003500090520     d Datasys         s               d   inz(*sys) datfmt(*iso)
003600090521     d WFDEMR          s             21    inz('GAITRAAZM /WFDEMR1L')
003700090520      *------------------------------------------------------------------------*
003800090520     D FNLV13R         pr                  extpgm('FNLV13R')
003900090520     D  kpjba                              likeds(kpjba)
004000090520     D  FNLV13DS                           likeds(FNLV13DS)
004100090520     D  TISI95DS                           likeds(TISI95DS)
004200090520     d
004300090520      /copy gaitrasrc/srcprotopr,trmk50r
004400050119      *------------------------------------------------------------------------*
004500090520     c     *entry        plist
004600090520     c                   parm                    kpjba
004700090520     c                   parm                    paramcpo         11
004800090520     c                   if        %parms=2 and paramcpo>*zeros
004900090520     c                   eval      wcpo=%int(paramcpo)
005000090520     c                   else
005100090520     c                   clear                   wcpo
005200090520     c                   endif
005300090521     c
005400090521     C                   OPEN(e)   wfdemr1l
005500090521     c
005600090521     c                   if        not %open(wfdemr1l)
005700090521     c                   eval      %subst(WFDEMR:7:4)='AZP '
005800090521     C                   OPEN      wfdemr1l
005900090521     c                   endif
006000080227      /free
006100090520       if wcpo>0    ;
006200090520       chain wcpo    tncpo01l ;
006300090520       if %found(tncpo01l)   ;
006400090520       exsr elabora    ;
006500090520       endif         ;
006600090520
006700090520       else   ;
006800090521       open qsysprt   ;
006900090521
007000090520       setll *loval  tncpo01l   ;
007100081215       read          tncpo01l   ;
007200080227
007300081215       dow  not %eof(tncpo01l)  ;
007400081215
007500090520       exsr   Elabora   ;
007600081215
007700081215       read          tncpo01l   ;
007800080227       enddo   ;
007900090520       endif   ;
008000090520
008100080227       Eval      *InLr = *On         ;
008200090520       //---------------------------------------------
008300090520       BEGSR  Elabora  ;
008400090520
008500090520       // Aggiorno data ultimo aggiornamento info
008600090520       //  e spesa trasporti se reale
008700090520
008800090520         clear ultdata  ;
008900090520         clear spesatra ;
009000090520         dcpo01=cporst   ;
009100090520         setll  cpocpo  ticpi01l ;
009200090520         reade  cpocpo  ticpi01l ;
009300090520
009400090520         dow not %eof(ticpi01l)    ;
009500090520         if cpidim>ultdata ;
009600090520         ultdata=cpidim  ;
009700090520         endif  ;
009800090520         if cpitpf='SPT'   ;
009900090520         spesatra=cpipft   ;
010000090520         endif  ;
010100090520
010200090520         reade  cpocpo  ticpi01l ;
010300090520         enddo ;
010400090520
010500090520         exsr Aggio    ;
010600090520         ENDSR  ;
010700081215       //---------------------------------------------
010800081215                    BEGSr Aggio   ;
010900090521                       chain(e)  cp1nrr    tncpo00f   ;
011000090521
011100090521                       if %error             ;
011200090521                       if wcpo=0    ;
011300090521                       except ERRORE   ;
011400090521                       endif    ;
011500090521
011600090521                       else ;
011700081215                      dcpo01=cporst    ;
011800090520
011900090520                      // Ultima data info comm
012000090520                      §cpoifodul=%editc(ultdata:'X')     ;
012100090520
012200090520                      // Spesa trasporti presunta/totale
012300090520                      if  spesatra=0         ;
012400090518                      exsr   calcol   ;
012500090518                      endif   ;
012600090518                      §cposptp =spesatra    ;
012700090520
012800090520                      // Info totali
012900090520                    clear trmk50ds    ;
013000090520                    i50cpo=cpocpo     ;
013100090520                    i50mod='C'        ;
013200090520                    i50obl='S'        ;
013300090520                    callp TRMK50R (kpjba:trmk50ds)  ;
013400090520                      §cpoifotot=o50ifotot   ;
013500090520
013600081215                      cporst=dcpo01    ;
013700081215                      update tncpo000  ;
013800090521                      endif  ;
013900090521
014000081215                      ENDSR   ;
014100090518       //--------------------------------------------------------------
014200090520       BEGSR Calcol   ;
014300090520 1     if  cpoftaz  >0 ;
014400090521       demtotfat = cpoftaz*1000   ;
014500090520
014600090520       clear demare   ;
014700090520
014800090520       // se presente la nazione, imposto la filiale del codice
014900090520       //  e non da cappario
015000090520 2     if CPOnaz<> '  '   ;
015100090520       DEMFIL=CPOflt    ;
015200090520 x2    ELSE   ;
015300090520
015400090520       clear   tisi95ds  ;
015500090520       clear   fnlv13ds  ;
015600090520       i95tcn='7'        ;
015700090520       i13la3='S'     ;
015800090520       i95cap=cpocap  ;
015900090520       i95loc=cpocit  ;
016000090520       i95prv=cpoprv  ;
016100090520       i95nar=cponaz  ;
016200090520       // data di elaborazione: 1 gennaio anno in corso
016300090520       i95dat=%dec(datasys )                         ;
016400090520       i13af0='S'     ;
016500090520       i13cnv=' '     ;
016600090520       i13af1=' '     ;
016700090520 3     if %subst(i95cap:1:2)='47 'and  i95prv='  ' ;
016800090520            I95PRV='RN'  ;
0169000905203        endif  ;
017000090520
017100090520       callp   FNLV13R  (kpjba:fnlv13ds:tisi95ds) ;
017200090520
017300090520       demfil=o95lna       ;
0174000905202      endif   ;
017500090520
017600090520       // se trovato imposto area
017700090520
0178000905202      if    demfil>0    ;
017900090520       chain   demfil   AZORG01L   ;
0180000905203      if %found(azorg01l) ;
018100090520         demare=orgcar ;
0182000905203      endif  ;
0183000905202      endif  ;
018400090520
018500090520
018600090520         alfasct=%editc(cposct:'X')     ;
018700090520         clear perc ;
018800090520
018900090520       // Ricavo l'area di appartenenza dall'indirizzo
019000090518
019100090520         chain (alfasct :demare)   wfdemr1l   ;
019200090518
019300090521       if %found(wfdemr1l)  ;
019400090520         exsr ScegliP    ;
019500090521       endif ;
019600090518
019700090518       if perc=0   ;
019800090520       clear demare    ;
019900090518       chain (alfasct : 000)   wfdemr1l   ;
020000090518
020100090521       if %found(wfdemr1l) ;
020200090518       exsr ScegliP    ;
020300090520       endif   ;
020400090521       endif   ;
020500090518
020600090518       if perc>0   ;
020700090521       eval(H) spesatra =(demtotfat *perc)/100   ;
020800090518
020900090518       // Arrotondo alle 1000 superiori
021000090518       w0080=spesatra /1000 ;
021100090518       w0080=w0080+1  ;
021200090518       spesatra =w0080*1000  ;
021300090518       endif   ;
021400090520       endif   ;
021500090518
021600090518       ENDSR   ;
021700090518       //--------------------------------------------------------------
021800090518       BEGSR ScegliP   ;
021900090518       select   ;
022000090521       when demtotfat<=piccoleAL  ;
022100090520       if r_dempcli>9  or demare=0  ;
022200090518       perc=r_dempperc  ;
022300090518       endif   ;
022400090521       when demtotfat<=MedieAL  ;
022500090520       if r_demmcli>9   or demare=0 ;
022600090518       perc=r_demmperc  ;
022700090518       endif   ;
022800090518       other  ;
022900090520       if r_demgcli>9   or demare=0 ;
023000090518       perc=r_demgperc  ;
023100090520       endif   ;
023200090518       endsl   ;
023300090518       ENDSR   ;
023400090518
023500080227
023600080227      /end-free
023700090521     oQsysprt   e            ERRORE      1
023800090521     o                                              'Potenziale non aggiornabil'
023900090521     o                                        +   0 'e perchè allocato'
024000090521     o                       cpOCPO           +   2
