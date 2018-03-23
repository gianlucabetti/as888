000100020925     h Decedit('0,') Datedit(*dmy.) option(*nodebugio)
000200050119      *------------------------------------------------------------------------*
000300110704     fTncpo01l  uf   e           K Disk
000400090521     fQsysprt   o    f  132        printer usropn
000500050119
000600050119      *------------------------------------------------------------------------*
000700050119      *   C A M P I   D I   L A V O R O
000800050119      *------------------------------------------------------------------------*
000900081215     d KPJBA         e ds
001000090520     d TRMK50ds      e ds                  inz
001100081215     d dcpo01        e ds                  inz
001200090520     d
001300090520      *------------------------------------------------------------------------*
001400090520     d
001500090520      /copy gaitrasrc/srcprotopr,trmk50r
001600050119      *------------------------------------------------------------------------*
001700090520     c     *entry        plist
001800090520     c                   parm                    kpjba
001900090520     c                   parm                    paramcpo         11
002000110704     c
002100090520     c                   if        %parms=2 and paramcpo>*zeros
002200090520     c                   eval      wcpo=%int(paramcpo)
002300090520     c                   else
002400110704     c                   clear                   wcpo             11 0
002500090520     c                   endif
002600090521     c
002700080227      /free
002800090520       if wcpo>0    ;
002900090520       chain wcpo    tncpo01l ;
003000090520       if %found(tncpo01l)   ;
003100110704       exsr Aggio      ;
003200090520       endif         ;
003300090520
003400090520       else   ;
003500090521       open qsysprt   ;
003600090521
003700090520       setll *loval  tncpo01l   ;
003800110704       read(e)       tncpo01l   ;
003900110704        if %error             ;
004000110704       read(n)       tncpo01l   ;
004100110704       endif  ;
004200080227
004300081215       dow  not %eof(tncpo01l)  ;
004400110704
004500110704        if %error             ;
004600110704         if wcpo=0    ;
004700110704            except ERRORE   ;
004800110704         endif    ;
004900110704
005000110704        // Lo rileggo non allocandolo e vado avanti
005100110704         read(n)       tncpo01l   ;
005200110704
005300110704        else ;
005400081215
005500110704         exsr   Aggio     ;
005600110704       endif            ;
005700081215
005800110704       read(e)       tncpo01l   ;
005900080227       enddo   ;
006000090520       endif   ;
006100090520
006200080227       Eval      *InLr = *On         ;
006300090520       //---------------------------------------------
006400081215                    BEGSr Aggio   ;
006500090521
006600081215                      dcpo01=cporst    ;
006700090520
006800090520                      // Info totali
006900090520                    clear trmk50ds    ;
007000090520                    i50cpo=cpocpo     ;
007100090520                    i50mod='C'        ;
007200090520                    i50obl='S'        ;
007300090520                    callp TRMK50R (kpjba:trmk50ds)  ;
007400090520                      §cpoifotot=o50ifotot   ;
007500090520
007600081215                      cporst=dcpo01    ;
007700081215                      update tncpo000  ;
007800090521
007900081215                      ENDSR   ;
008000090518
008100080227
008200080227      /end-free
008300090521     oQsysprt   e            ERRORE      1
008400090521     o                                              'Potenziale non aggiornabil'
008500090521     o                                        +   0 'e perchè allocato'
008600090521     o                       cpOCPO           +   2
