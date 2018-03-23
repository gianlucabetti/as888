000100990614     h DECEDIT('0,') DATEDIT(*DMY.)
000200980323     H*------------------------------------------------------------------------*
000300001122     H* PULIZIA ARCHIVIO DETTAGLIO TARIFFE PARTICOLARI                         *
000400980323     H*------------------------------------------------------------------------*
000500980323     F*
000600011122     FTITPT00f  ip   e             DISK
000700011122     FTITPD01l  iF   e           k DISK
000800990721     FTNTAM01l  if   e           k DISK
000900001122     fPRTF198   O    F  132        PRINTER  oflind(*INOF)
001000990721      *
001100001122     d savksc          s                   like(tptksc)
001200001122     d savctr          s                   like(tptctr)
001300001122     d savprg          s                   like(tptprg)
001400001122     d savftc          s                   like(tptftc)
001500990721      *
001600990721     c     keytam        klist
001700001122     c                   kfld                    tptksc
001800001122     c                   kfld                    tptctr
001900001122     c                   kfld                    tptprg
002000001122      *
002100001122     c     keytpd        klist
002200001122     c                   kfld                    tptksc
002300001122     c                   kfld                    tptctr
002400001122     c                   kfld                    tptprg
002500001122     c                   kfld                    tptftc
002600001122      *
002700001122      * ogni volta che cambia la chiave aggancio tntam
002800001122     c                   if        savksc <> tptksc or savctr <> tptctr or
002900001122     c                             savprg <> tptprg or savftc <> tptftc
003000001122     c                   eval      savksc = tptksc
003100001122     c                   eval      savctr = tptctr
003200001122     c                   eval      savprg = tptprg
003300001122     c                   eval      savftc = tptftc
003400990614      *
003500990820      * aggancio la testata
003600990820     c     keytam        chain     tntam01l                           30
003700001122     c                   if        not *in90 or *inof
003800001122     c                   except    testa
003900001122     c                   seton                                        90
004000001122     c                   setoff                                       of
004100001122     c                   endif
004200001122      *
004300001122     c                   endif
004400001122      *
004500001122     c                   if        (not *in30 and tamatb = 'A' and tptatb = ' ')
004600001122     c                             or *in30
004700001122     c                   except    detta
004800001122      *
004900001122     c                   endif
005000001122      *
005100001123     c                   if        (not *in30 and tamatb = 'A' and tptatb = ' ')
005200001123     c                             or *in30
005300001123      * se esiste la testata ma è annullata annullo anche il record tariffe particolari
005400001123     c                   eval      tptatb = 'A'
005500001123     c                   clear                   tptftr
005600001123     c                   clear                   tptdtr
005700011122     c**!!**             update    titpt000
005800001122      * aggiorno anche il dettaglio tariffe particolari
005900001122     c     keytpd        setll     titpd01l
006000001122     c                   do        *hival
006100001122     c     keytpd        reade     titpd01l
006200001122      * fine file
006300001122     c                   if        %eof(titpd01l)
006400001122     c                   leave
006500001122     c                   endif
006600001122      * aggiorno il dettaglio tariffe particolari
006700001123     c                   eval      tpdatb = 'A'
006800001123     c                   clear                   tpdftr
006900001123     c                   clear                   tpddtr
007000011122     c**!!**             update    titpd000
007100001122     c                   enddo
007200001122     c                   endif
007300001122      * se non esiste deleto il record
007400001123     c*!!*               if        *in30
007500001122     c*!!*               delete    titpt000
007600001122      * aggiorno anche il dettaglio tariffe particolari
007700001123     c*!!* keytpd        setll     titpd01l
007800001123     c*!!*               do        *hival
007900001123     c*!!* keytpd        reade     titpd01l
008000001122      * fine file
008100001123     c*!!*               if        %eof(titpd01l)
008200001123     c*!!*               leave
008300001123     c*!!*               endif
008400001122      * deleto il dettaglio tariffe particolari
008500001122     c*!!*               delete    titpd000
008600001123     c*!!*               enddo
008700001123     c*!!*               endif
008800990614      *
008900001122      *------------------------------------------------------------
009000001122     OPRTF198   E            testa            01
009100001122     O                                        +   1 'Dettaglio Tariffario (TITA'
009200001122     O                                              'D) senza testata o con tes'
009300001122     o                                              'tata annullata'
009400001122     O          E            testa            03
009500001122     O                                        +   1 'Cliente tariffa  prog TC'
009600001122     O                                        +   4 'Anomalia'
009700001122     O          E            detta       1
009800001122     O                       tptksc           +   1
009900001122     O                       tptctr           +   3
010000001122     O                       tptprg           +   4
010100001122     O                       tptftc           +   2
010200001122     O               30                          51 'Non Esiste la tariffa'
010300001122     O              N30                          47 'Tariffa Annullata    '
