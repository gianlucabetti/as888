000100990614     h DECEDIT('0,') DATEDIT(*DMY.)
000200980323     H*------------------------------------------------------------------------*
000300031112     H* PULIZIA ARCHIVIO TESTATA   TARIFFE PARTICOLARI                         *
000400980323     H*------------------------------------------------------------------------*
000500980323     F*
000600031112     FTITPT00f  UP   e             DISK
000700990721     FTNTAM01l  if   e           k DISK
000800001122     fPRTF198   O    F  132        PRINTER  oflind(*INOF)
000900990721      *
001000001122     d savksc          s                   like(tptksc)
001100001122     d savctr          s                   like(tptctr)
001200001122     d savprg          s                   like(tptprg)
001300001122     d savftc          s                   like(tptftc)
001400990721      *
001500990721     c     keytam        klist
001600001122     c                   kfld                    tptksc
001700001122     c                   kfld                    tptctr
001800001122     c                   kfld                    tptprg
001900001122      *
002000001122      * ogni volta che cambia la chiave aggancio tntam
002100001122     c                   if        savksc <> tptksc or savctr <> tptctr or
002200001122     c                             savprg <> tptprg or savftc <> tptftc
002300001122     c                   eval      savksc = tptksc
002400001122     c                   eval      savctr = tptctr
002500001122     c                   eval      savprg = tptprg
002600001122     c                   eval      savftc = tptftc
002700990614      *
002800990820      * aggancio la testata
002900990820     c     keytam        chain     tntam01l                           30
003000001122     c                   if        not *in90 or *inof
003100001122     c                   except    testa
003200001122     c                   seton                                        90
003300001122     c                   setoff                                       of
003400001122     c                   endif
003500001122      *
003600001122      *
003700001122     c                   if        (not *in30 and tamatb = 'A' and tptatb = ' ')
003800001122     c                             or *in30
003900001122     c                   except    detta
004000001122      *
004100001122     c                   endif
004200001122      *
004300001123     c                   if        (not *in30 and tamatb = 'A' and tptatb = ' ')
004400001123      * se esiste la testata ma è annullata annullo anche il record tariffe particolari
004500001123     c                   eval      tptatb = 'A'
004600051107     c                   eval      tptdtr = tamdtr
004700031112     c                   update    titpt000
004800001122     c                   endif
004900001122      * se non esiste deleto il record
005000031112     c                   if        *in30
005100031112     c                   delete    titpt000
005200031112     c                   endif
005300031112     c                   endif
005400990614      *
005500001122      *------------------------------------------------------------
005600001122     OPRTF198   E            testa            01
005700031112     O                                        +   1 'Dettaglio Tariffario (TITP'
005800031112     O                                              'T) senza testata o con tes'
005900001122     o                                              'tata annullata'
006000001122     O          E            testa            03
006100001122     O                                        +   1 'Cliente tariffa  prog TC'
006200001122     O                                        +   4 'Anomalia'
006300001122     O          E            detta       1
006400001122     O                       tptksc           +   1
006500001122     O                       tptctr           +   3
006600001122     O                       tptprg           +   4
006700001122     O                       tptftc           +   2
006800001122     O               30                          51 'Non Esiste la tariffa'
006900001122     O              N30                          47 'Tariffa Annullata    '
