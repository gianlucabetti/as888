000100041126     H DECEDIT('0,') DATEDIT(*DMY.)
000200041126     H*         *----------------------------------------------------*
000300041126     H*       comodo
000400041126     H*--------------------------------------------------------------*
000500041126     FFNCLS01L  uf   e           k disk    usropn
000600041126     fcnaco00f  uf   e           k disk    usropn
000700041126      *
000800041126     fprtf198   o    f  132        printer  oflind(*inof)
000900041126      *
001000041126     d tot_V           s              9  0 inz(0)
001100041126     d tot_M           s              9  0 inz(0)
001200041126
001300041126     d keycut          s                   like(acokut) inz(1)
001400041126     d keykcc          s                   like(acokcc) inz(0151)
001500041126     d keyksc          s                   like(acoksc)
001600041126
001700041126     D                 DS
001800041126     D  VIDtfg                 1      1
001900041126     D  VIDtcm                10     10
002000041126     D  FLG                    1     10
002100041126     D                                     DIM(10)
002200041126     c     *entry        plist
002300041126     c                   parm                    errore            2
002400041126      *
002500041126     c                   clear                   errore
002600041126      *
002700041126     c                   open(E)   cnaco00f
002800041126     c                   if        %error
002900041126     c                   move      'ER'          errore
003000041126     c                   goto      fine
003100041126     c                   end
003200041126      *
003300041126     c                   open(E)   FNCLS01L
003400041126     c                   if        %error
003500041126     c                   move      'ER'          errore
003600041126     c                   goto      fine
003700041126     c                   end
003800041126      *
003900041126     c     keyaco        klist
004000041126     c                   kfld                    keycut
004100041126     c                   kfld                    keykcc
004200041126     c                   kfld                    keyksc
004300041126
004400041126     c                   except    testa
004500041126
004600041126     c                   do        *hival
004700041126     c                   read      fncls01l
004800041126      *
004900041126     c                   if        %eof(fncls01l)
005000041126     c                   leave
005100041126     c                   endif
005200041126      *
005300041126     c                   movea     clsflo        flg
005400041126      *
005500041126      * il 1° flag è la chiusura
005600041126      * il 10° indica l'apertura
005700041126     c                   if        flg(10) = 'V' and flg(1) = 'X' or
005800041126     c                             flg(10) = 'V' and flg(1) = 'F'
005900041126      *  in chiusura
006000041126      * V = E-mail singola                 se  X = fax singolo
006100041126      * M = E-mail riepilogativa           se  F = fax riepilogo
006200041126      *
006300041126     c                   clear                   cosa_era          1
006400041126     c                   if        flg(1) = 'X'
006500041126     c                   move      'X'           cosa_era          1
006600041126     c                   movel     'V'           clsflo
006700041126     c                   add       1             Tot_V
006800041126     c                   end
006900041126      *
007000041126     c                   if        flg(1) = 'F'
007100041126     c                   move      'F'           cosa_era          1
007200041126     c                   movel     'M'           clsflo
007300041126     c                   add       1             Tot_M
007400041126     c                   end
007500041126      *
007600041126     c                   update    fncls000
007700041126      *
007800041126      * sflaggo l'anagrafica  clienti
007900041126     c                   z-add     clsksc        keyksc
008000041126     c                   clear                   aco_agg           5
008100041126     c     keyACO        chain     cnaco00f
008200041126     c                   if        %found(cnaco00f)
008300041126     c                   clear                   acoftr
008400041126     c                   clear                   acodtr
008500041126     c                   update    cnaco000
008600041126     c                   else
008700041126     c                   move      'N_ACO'       aco_agg
008800041126     c                   endif
008900041126
009000041126     c                   if        *inof
009100041126     c                   except    testa
009200041126     c                   setoff                                       of
009300041126     c                   endif
009400041126     c                   except    riga
009500041126      *
009600041126     c                   end
009700041126      *
009800041126     c                   enddo
009900041126      *
010000041126      * stampo i totali
010100041126     c                   except    totali
010200041126
010300041126     c     fine          tag
010400041126     c                   seton                                        lr
010500041126      *
010600041126      *---------------------------------------------------------------------------------------------
010700041126     OPRTF198   E            testa            01
010800041126     O                                        +   1 'Aggiorna Flag Comunicaz.Ch'
010900041126     O                                              'iusura Giacenza - in base'
011000041126     O                                              ' al Flag di Apertura con'
011100041126     O                                              ' E-mail'
011200041126     O          e            riga        1
011300041126     O                       clsksc        Z     +2
011400041126     O                       cosa_era            +3
011500041126     O                       aco_agg             +3
011600041126     O          e            totali      2
011700041126     O                                        +   1 'T O T A L I  -  '
011800041126     O                                        +   1 'aagiornati  : '
011900041126      *
012000041126     O                                        +   1 'con (V) E.mail singola:'
012100041126     O                       tot_V         z  +   0
012200041126     O                                        +   2 'con (M) E.mail riepilogo:'
012300041126     O                       tot_M         z  +   0
