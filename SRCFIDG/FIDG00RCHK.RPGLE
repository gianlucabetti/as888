000100040309      /TITLE Controllo attività UpLoad
000200990908     H dftactgrp(*yes)
000300990907
000400070604     Ffilte03l  if   e           k disk
000500070604
000600070604     D* Variabili di wrk
000700070604     D  datoracorTim   s               z
000800070604     D  datoradwlTim   s               z
000900070604     D  datoratstTim   s               z
001000070604
001100070604     D*------------------
001200070604     D* DS "XSRDA8" - CONTROLLA DATA (8)
001300070604     D*------------------
001400070604     D WLBDA8          DS                  INZ
001500070604     D  G08DAT                 1      8  0
001600070604     D  G08INV                 9     16  0
001700070604     D  G08ERR                17     17
001800070604     D  G08TGI                18     22  0
001900000710
002000070604
002100070604     C                   setoff                                       10
002200130613     C                   z-add     *zeros        wMinLock          5 0
002300070604     C*
002400070604     C                   dou       *inlr
002500070604     C*
002600070604     C                   read      filte03l                               lr
002700070604     C*
002800070604     C                   if        not *inlr
002900070604     C* Gestione ENDJOB.
003000070604     C                   shtdn                                        lr
003100070604     C   lr              return
003200070604     C*
003300070604     C* Solo record log nn annullati
003400080121     C                   if        ltehdl='ANNULLATO' OR
003500080121     C                             ltehdl='IN CORSO'
003600070604     C                   else
003700070604     C*
003800070604     C* Calcolo da data/ora corrente
003900070604     C                   time                    wn14             14 0
004000070604     C                   move      wn14          wn8               8 0          *DATA 8  GG/MM/AAAA
004100070604     C                   movel     wn14          oracor            6 0          *ORA  6  HH/MM/SS
004200070604     C                   z-add     wn8           g08dat
004300070604     C                   z-add     *zeros        g08inv
004400070604     C                   movel     '0'           g08err
004500070604     C                   call      'XSRDA8'
004600070604     C                   parm                    wlbda8
004700070604     C                   z-add     g08inv        datcor            8 0          *DATA COR AAAA/MM/GG
004800070604     C                   movel(p)  datcor        datora           14 0          *DATA COR + ORA COR
004900070604     C                   move      oracor        datora
005000070604     C                   movel(p)  *all'0'       datora_z         20 0
005100070604     C                   movel     datora        datora_z
005200070604     C                   eval      datoracorTim = %timestamp(datora_z:*iso)
005300070604     C*
005400070604     C* Reperisco la data/ora d download della transazione Tellus
005500070604     C                   movel(p)  lteDAT        datora
005600070604     C                   move      lteORA        datora
005700070604     C                   movel(p)  *all'0'       datora_z
005800070604     C                   movel     datora        datora_z
005900070604     C                   eval      datoradwlTim = %timestamp(datora_z:*iso)
006000070604     C*
006100070604     C* Verifico se passato + d <n> minuti dalla data/ora download
006200070604     C                   eval      datoratstTim = datoradwlTim + %minutes(upl)
006300070604     C                   if        datoratstTim < datoracorTim
006400070604     C                   seton                                        10
006500130613     C                   add       1             wMinLock
006600070604     C                   endif
006700070604     C                   endif
006800070604     C                   endif
006900070604     C*
007000070604     C                   enddo
007100070604     C*
007200070604     C* Se Tellus risulta fermo da + d <n> minuti => mando sms e mail
007300130613     C                   if        *in10 = *on AND wMinLock > 3
007400070604     C                   exsr      sndeml
007500070604     C                   exsr      sndsms
007600070604     C                   endif
007700070604     C*--------------------------------------------------------------------
007800070604     C* ...and that's all folks!
007900070604     C*--------------------------------------------------------------------
008000040309
008100070604
008200070604
008300040309
008400070604      /TITLE Compongo il testo e spedisco gli sms
008500070604     C     sndsms        begsr
008600040309     C*
008700040309     C* Inizializzo variabili
008800070604     C                   movel     *blanks       wrkSms          150
008900070604     C* Valorizzo i testo del messaggio
009000070604     C                   eval      wrkSms = 'TELLUS: attenzione verificare ' +
009100070604     C                             'funzionamento delle procedure su ' +
009200070604     C                             'server.'
009300040309     C*
009400070604     C                   call(e)   'TNSYEMSC'
009500070604     C                   parm                    wrkSms
009600040309     C*
009700040309     C                   endsr
009800070604
009900070604
010000070604
010100070604
010200070604      /TITLE Compongo il testo e spedisco una e-m@ail
010300070604     C     sndeml        begsr
010400070604     C*
010500070604     C* Inizializzo variabili
010600070604     C                   movel     *blanks       wrkEml          253
010700070604     C                   movel     *blanks       wrkMsg         5000
010800070604     C                   movel     *blanks       wrkOgg           44
010900070604     C* Valorizzo i campi della e-m@ail
011000120305     C                   eval      wrkEml='cedalert@brt.it'
011100070604     C                   eval      wrkOgg='TELLUS - WARNING!!!'
011200070604     C                   eval      wrkMsg='ATTENZIONE: '               +':/N'+
011300070604     C                             'il numero dei file da elaborare '  +
011400070604     C                             'non diminuisce!'                   +':/N'+
011500070604     C                             'Verificare il corretto '           +
011600070604     C                             'funzionamento delle procedure su ' +
011700070604     C                             'server.'
011800070604     C*
011900070604     C                   call(e)   'TIS701C'
012000070604     C                   parm                    wrkEml
012100070604     C                   parm                    wrkOgg
012200070604     C                   parm                    wrkMsg
012300070604     C*
012400070604     C                   endsr
012500000714
012600070604
012700070604
012800000714
012900000714      /TITLE Routine di *init PGM
013000000714     C*
013100000714     C     *inzsr        begsr
013200070604     C*
013300070604     C     *entry        plist
013400070604     C                   parm                    uplalf            3
013500070604     C*
013600070604     C                   move(p)   uplalf        upl               3 0
013700000714     C*
013800000714     C                   endsr
013900000714
