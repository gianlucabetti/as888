000100990614     h DECEDIT('0,') DATEDIT(*DMY.)
000200980323     H*------------------------------------------------------------------------*
000300001122     H* PULIZIA ARCHIVIO DETTAGLIO TARIFFE                                     *
000400980323     H*------------------------------------------------------------------------*
000500980323     F*
000600051107     FTITGC00f  up   e             DISK
000700990721     FTNTAM01l  if   e           k DISK
000800001122     fPRTF198   O    F  132        PRINTER  oflind(*INOF)
000900990721      *
001000001122     d savksc          s                   like(tgcksc)
001100001122     d savctr          s                   like(tgcctr)
001200001122     d savprg          s                   like(tgcprg)
001300990721      *
001400990721     c     keytam        klist
001500001122     c                   kfld                    tgcksc
001600001122     c                   kfld                    tgcctr
001700001122     c                   kfld                    tgcprg
001800001122      *
001900001122      * ogni volta che cambia la chiave aggancio tntam
002000001122     c                   if        savksc <> tgcksc or savctr <> tgcctr or
002100001122     c                             savprg <> tgcprg
002200001122     c                   eval      savksc = tgcksc
002300001122     c                   eval      savctr = tgcctr
002400001122     c                   eval      savprg = tgcprg
002500990614      *
002600990820      * aggancio la testata
002700990820     c     keytam        chain     tntam01l                           30
002800001122     c                   if        not *in90 or *inof
002900001122     c                   except    testa
003000001122     c                   seton                                        90
003100001122     c                   setoff                                       of
003200001122     c                   endif
003300001122      * STAMPO GLI ERRORI
003400001122     c                   if        (not *in30 and tamatb = 'A' and tgcatb = ' ')
003500051107     c                             or *in30
003600001122     c                   except    detta
003700001122     c                   endif
003800001122      *
003900001122      *
004000001122     c                   endif
004100001122      *
004200001123     c                   if        (not *in30 and tamatb = 'A' and tgcatb = ' ')
004300001122      * se esiste la testata ma è annullata annullo anche il record giacenza
004400001123     c                   eval      tgcatb = 'A'
004500001123     c                   clear                   tgcftr
004600051107     c                   eval      tgcdtr =tamdtr
004700051107     c                   update    titgc000
004800001122     c                   endif
004900001122      * se non esiste deleto il record
005000051107     c                   if        *in30
005100051107     c                   delete    titgc000
005200051107     c                   endif
005300990614      *
005400001122      *------------------------------------------------------------
005500001122     OPRTF198   E            testa            01
005600001122     O                                        +   1 'Tariffe Giacenze     (TITG'
005700001122     O                                              'C) senza testata o con tes'
005800001122     o                                              'tata annullata'
005900001122     OPRTF198   E            testa            03
006000001122     O                                        +   1 'Cliente tariffa  prog'
006100001122     O                                        +   4 'Anomalia'
006200001122     O          E            detta       1
006300001122     O                       tgcksc           +   1
006400001122     O                       tgcctr           +   3
006500001122     O                       tgcprg           +   4
006600001122     O               30                          47 'Non Esiste la tariffa'
006700001122     O              N30                          43 'Tariffa Annullata    '
