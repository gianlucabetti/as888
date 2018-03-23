000100990614     h DECEDIT('0,') DATEDIT(*DMY.)
000200980323     H*------------------------------------------------------------------------*
000300031112     H* PULIZIA ARCHIVIO dettaglio TARIFFE PARTICOLARI                         *
000400980323     H*------------------------------------------------------------------------*
000500980323     F*
000600031112     FTITPD00f  uP   e             DISK
000700990721     FTNTAM01l  if   e           k DISK
000800001122     fPRTF198   O    F  132        PRINTER  oflind(*INOF)
000900990721      *
001000031112     d savksc          s                   like(tpDksc)
001100031112     d savctr          s                   like(tpDctr)
001200031112     d savprg          s                   like(tpDprg)
001300031112     d savftc          s                   like(tpDftc)
001400990721      *
001500990721     c     keytam        klist
001600031112     c                   kfld                    tpdksc
001700031112     c                   kfld                    tpdctr
001800031112     c                   kfld                    tpdprg
001900001122      *
002000001122      * ogni volta che cambia la chiave aggancio tntam
002100031112     c                   if        savksc <> tpdksc or savctr <> tpdctr or
002200031112     c                             savprg <> tpdprg or savftc <> tpdftc
002300031112     c                   eval      savksc = tpdksc
002400031112     c                   eval      savctr = tpdctr
002500031112     c                   eval      savprg = tpdprg
002600031112     c                   eval      savftc = tpdftc
002700990614      *
002800990820      * aggancio la testata
002900990820     c     keytam        chain     tntam01l                           30
003000001122     c                   if        not *in90 or *inof
003100001122     c                   except    testa
003200001122     c                   seton                                        90
003300001122     c                   setoff                                       of
003400001122     c                   endif
003500031112      *
003600031112     c                   endif
003700001122      *
003800031112     c                   if        (not *in30 and tamatb = 'A' and tpdatb = ' ')
003900001122     c                             or *in30
004000031112     c                   add       1             cont              4 0
004100001122     c                   except    detta
004200001122      *
004300001122     c                   endif
004400001122      *
004500031112     c                   if        (not *in30 and tamatb = 'A' and tpdatb = ' ')
004600001123      * se esiste la testata ma è annullata annullo anche il record tariffe particolari
004700031112     c                   eval      tpdatb = 'A'
004800051107     c                   eval      tpddtr = tamdtr
004900031112     c                   update    titpd000
005000001122     c                   endif
005100001122      * se non esiste deleto il record
005200031112     c                   if        *in30
005300031112     c                   delete    titpd000
005400031112     c                   endif
005500990614      *
005600001122      *------------------------------------------------------------
005700001122     OPRTF198   E            testa            01
005800031112     O                                        +   1 'Dettaglio Tariffario (TITP'
005900031112     O                                              'D) senza testata o con tes'
006000001122     o                                              'tata annullata'
006100001122     O          E            testa            03
006200001122     O                                        +   1 'Cliente tariffa  prog TC'
006300001122     O                                        +   4 'Anomalia'
006400001122     O          E            detta       1
006500031112     O                       tpdksc           +   1
006600031112     O                       tpdctr           +   3
006700031112     O                       tpdprg           +   4
006800031112     O                       tpdftc           +   2
006900001122     O               30                          51 'Non Esiste la tariffa'
007000001122     O              N30                          47 'Tariffa Annullata    '
007100031112     O                       cont          z     90
