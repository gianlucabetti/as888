000100990614     h DECEDIT('0,') DATEDIT(*DMY.)
000200980323     H*------------------------------------------------------------------------*
000300990611     H* PREP. ARCHIVIO DETTAGLIO TARIFFE                                       *
000400980323     H*------------------------------------------------------------------------*
000500980323     F*
000600990614     FTNTAD00f  ip   e             DISK
000700990721     FTNTAM01l  if   e           k DISK
000800990721     FTABEL00f  if   e           k DISK
000900990820     FTITAD04l  uf a e           k DISK
001000990721     F                                     prefix(o)
001100990721      *
001200990721     d cod             s              2    inz('CT')
001300990721     d key             s              8
001400990721     d codut           s              1  0 inz(1)
001500990614      *
001600990721     D DSCT          E DS
001700990721      *
001800990721     c     keytam        klist
001900990721     c                   kfld                    tadksc
002000990721     c                   kfld                    tadctr
002100990721     c                   kfld                    tadprg
002200990820      *
002300990820     c     keytad        klist
002400990820     c                   kfld                    otadksc
002500990820     c                   kfld                    otadctr
002600990820     c                   kfld                    otadprg
002700990820     c                   kfld                    otadlnp
002800990820     c                   kfld                    otadorp
002900990820     c                   kfld                    otadnaz
003000990820     c                   kfld                    otadcap
003100990820     c                   kfld                    otadsgl
003200990721      *
003300990721     c     keytab        klist
003400990721     c                   kfld                    codut
003500990721     c                   kfld                    cod
003600990721     c                   kfld                    key
003700990721      *
003800990614     c                   clear                   titad000
003900990614      *
004000990614     c                   eval      otadksc = tadksc
004100990614     c                   eval      otadctr = tadctr
004200990614     c                   eval      otadprg = tadprg
004300990614     c                   eval      otadlnp = tadlnp
004400990820     c                   move      tadorp        otadorp
004500990614     c                   eval      otadcap = tadcap
004600990820      * trascodifico la nazione in base al Codice tassazione solo se inserito il
004700990820      * CAP ed è tariffa estera
004800990820     c                   if        tadcap <> *blank
004900990820      * aggancio la testata
005000990820     c     keytam        chain     tntam01l                           30
005100990820     c                   if        not *in30 and tamfie = 'E'
005200990820      * aggancio la tabella per codice tassazione
005300990820     c                   movel     tadcts        key
005400990820     c     keytab        chain     tabel00f                           31
005500990820     c                   if        not *in31
005600990820     c                   movel     tbluni        dsct
005700990820     c                   eval      otadnaz = §ctnar
005800990820     c                   endif
005900990820      *
006000990820     c                   endif
006100990820      *
006200990820     c                   endif
006300990614     c                   eval      otadsgl = tadsgl
006400990820     c* verifico se esiste il record sul file
006500990820     c     keytad        chain     titad04l                           30
006600990820     c                   if        *in30
006700990820     c                   movel     tadcts        otadcts
006800990614     c                   eval      otaditr = taditr
006900990614     c                   eval      otadino = tadino
007000990614     c                   eval      otadrpv = tadrpv
007100990820     c                   eval      otadatb = tadatb
007200990614     c                   z-add     tadmin        otadmin
007300990614     c                   z-add     tadmax        otadmax
007400990614      * se flag trasmissione uguale a 'T' lo pulisco e trasmetto
007500990614     c                   if        tadftr = 'T'
007600990614     c                   else
007700990614     c                   eval      otadftr = tadftr
007800990614     c                   endif
007900990614      *
008000990614     c                   eval      otaddtr = taddtr
008100990614      *
008200990614     c                   write     titad000
008300990820      *
008400990820     c                   endif
008500990614      *
