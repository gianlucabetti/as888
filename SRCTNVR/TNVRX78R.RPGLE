000100000601     H DECEDIT('0,') DATEDIT(*DMY.)
000200000601      *
000300150408     ffnarb01l  IF   E           K DISK    USROPN EXTfile(wfnarb)
000400150408     fazorg01l  IF   E           K DISK
000500150408      * ds per il controllo della presenza di CA per la spedizione richiesta
000600150408     d FIDN12DS      e ds
000700150408     d  skKey                 26    305    dim(20)
000800150408     d  skAnn                306    325    dim(20)
000900150408     d  skDca                326    485  0 dim(20)
001000150408     d  skFca                486    545  0 dim(20)
001100150408     d  skDch                546    705  0 dim(20)
001200150408     d  skCch                706    745    dim(20)
001300150408     d*
001400150408     d dsKey           ds
001500150408     d  dsaac                         4  0
001600150408     d  dsfil                         3  0
001700150408     d  dsnca                         7  0
001800150408
001900150408     d                 ds
002000150408     d  arblnp                 1      3  0
002100150408     d  arbnrs                 4      5  0
002200150408     d  arbnsp                 6     12  0
002300150408     d  spediz                 1     12  0
002400150408     d Wfnarb          s             21    inz('FILTRA201/FNARB01L')
002500160817     d Dataiso         s               d   datfmt(*iso) inz(*sys)
002600000602      *--------------------------------------------------
002700150409     c     karb          klist
002800150409     c                   kfld                    kaas              4 0
002900150409     c                   kfld                    klnp              3 0
003000150409     c     *entry        plist
003100160817     c                   parm                    AASlnp            7
003200150409     c                   clear                   klnp
003300160817     c                   clear                   kaas
003400150409     c
003500150408     c                   open      fnarb01l
003600160817     c                   if        AASlnp>*zeros
003700160817     c                   moveL     AASLNP        kaas
003800160817     c                   move      aaslnp        klnp
003900150409     c                   endif
004000021216      *
004100150409     c                   if        klnp>0
004200150409     c     karb          setll     fnarb01l
004300160817     c     KARB          reade     fnarb01l
004400150409     c                   else
004500160817     c                   movel     dataiso       w0080             8 0
004600160817     c                   movel     w0080         kaas
004700160817     c     kaas          setll     fnarb01l
004800160817     c     kaas          reade     fnarb01l
004900150409     c                   endif
005000150409     c
005100150408    4c                   dow       not %eof(fnarb01l)
005200150408     c                   if        arbdcm=0 and arblna<>101
005300150408     c     arblna        chain     azorg01l
005400170310     c***                if        orgfl3<>'7'
005500150408     c* Escludo se ha c.a.
005600150408     c                   clear                   fidn12ds
005700150408     c                   eval      i12aas = arbAAS
005800150408     c                   eval      i12lnp = arbLNP
005900150408     c                   eval      i12nrs = arbNRS
006000150408     c                   eval      i12nsp = arbNSP
006100150408     c                   eval      i12fel = 'E'
006200150408     c                   eval      i12fan = 'E'
006300150408     c*
006400150408     c                   call      'FIDN12R'
006500150408     c                   parm                    fidn12ds
006600150408     c*
006700150408     c                   if        o12err =  ' '    and
006800150408     c                             o12nca = 0
006900150408     c                   call      'TNVRX77R'
007000150408     c                   parm                    spediz
007100150408     c                   parm      ' '           esito             1
007200150408     c                   endif
007300150408     c                   endif
007400170310     c*****              endif
007500150408     c
007600150409     c                   if        klnp>0
007700150409     c     karb          reade     fnarb01l
007800150409     c                   else
007900160817     c     kaas          reade     fnarb01l
008000150409     c                   endif
008100120111    1c                   ENDDO
008200021213      *
008300021213     C                   eval       *inlr = *on
008400150408     c* Richiamo il *pgm d utilità FIDN12R in modalità "solo chiusura"
008500150408     c                   clear                   fidn12ds
008600150408     c                   eval      i12tla = 'C'
008700150408     c                   call      'FIDN12R'
008800150408     c                   parm                    fidn12ds
