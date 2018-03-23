000100990614     h DECEDIT('0,') DATEDIT(*DMY.)
000200980323     H*------------------------------------------------------------------------*
000300001122     H* PULIZIA ARCHIVIO DETTAGLIO TARIFFE                                     *
000400980323     H*------------------------------------------------------------------------*
000500980323     F*
000600011122     FTITAD00f  ip   e             DISK
000700990721     FTNTAM01l  if   e           k DISK
000800001122     fPRTF198   O    F  132        PRINTER  oflind(*INOF)
000900990721      *
001000001122     d savksc          s                   like(tadksc)
001100001122     d savctr          s                   like(tadctr)
001200001122     d savprg          s                   like(tadprg)
001300990721      *
001400990721     c     keytam        klist
001500990721     c                   kfld                    tadksc
001600990721     c                   kfld                    tadctr
001700990721     c                   kfld                    tadprg
001800001122      *
001900001122      * ogni volta che cambia la chiave aggancio tntam
002000001122     c                   if        savksc <> tadksc or savctr <> tadctr or
002100001122     c                             savprg <> tadprg
002200001122     c                   eval      savksc = tadksc
002300001122     c                   eval      savctr = tadctr
002400001122     c                   eval      savprg = tadprg
002500990614      *
002600990820      * aggancio la testata
002700990820     c     keytam        chain     tntam01l                           30
002800001122     c                   if        not *in90 or *inof
002900001122     c                   except    testa
003000001122     c                   seton                                        90
003100001122     c                   setoff                                       of
003200001122     c                   endif
003300001122      * STAMPO GLI ERRORI
003400001122     c                   if        (not *in30 and tamatb = 'A' and tadatb = ' ')
003500001122     c                             or *in30
003600001122     c                   except    detta
003700001122     c                   endif
003800001122      *
003900001122      *
004000001122     c                   endif
004100001122      *
004200001123     c                   if        (not *in30 and tamatb = 'A' and tadatb = ' ')
004300001123     c                              or *in30
004400001122      * se esiste la testata ma è annullata annullo anche il record dettaglio tariffario
004500001123     c                   eval      tadatb = 'A'
004600001123     c                   clear                   tadftr
004700001123     c                   clear                   taddtr
004800011122     c*!!*               update    titad000
004900001122     c                   endif
005000001122      * se non esiste deleto il record
005100001123     c*!!*               if        *in30
005200001122     c*!!*               delete    titad000
005300001123     c*!!*               endif
005400001123      *
005500001123      *------------------------------------------------------------
005600001122     OPRTF198   E            testa            01
005700001122     O                                        +   1 'Dettaglio Tariffario (TITA'
005800001122     O                                              'D) senza testata o con tes'
005900001122     o                                              'tata annullata'
006000001122     OPRTF198   E            testa            03
006100001122     O                                        +   1 'Cliente tariffa  prog'
006200001122     O                                        +   4 'Anomalia'
006300001122     O          E            detta       1
006400001122     O                       tadksc           +   1
006500001122     O                       tadctr           +   3
006600001122     O                       tadprg           +   4
006700001122     O               30                          47 'Non Esiste la tariffa'
006800001122     O              N30                          43 'Tariffa Annullata    '
