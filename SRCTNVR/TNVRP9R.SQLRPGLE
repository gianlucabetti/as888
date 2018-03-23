000100051229      *PARMS CLOSQLCSR(*ENDMOD)
000200051229      *PARMS DYNUSRPRF(*OWNER)
000300051227      *-----------------------------------------------------------------*
000400060405      *  Elabora calendario eventi particolari per scrivere FNEVB
000500051227      *-----------------------------------------------------------------*
000600051227
000700060405     D                 DS
000800051227     d kpjba         e ds
000900060830      * ds per il controllo della presenza di CA per la spedizione richiesta
001000060830     d FIDN12DS      e ds
001100060830     d  skKey                 26    305    dim(20)
001200060830     d  skAnn                306    325    dim(20)
001300060830     d  skDca                326    485  0 dim(20)
001400060830     d  skFca                486    545  0 dim(20)
001500060830     d  skDch                546    705  0 dim(20)
001600060830     d  skCch                706    745    dim(20)
001700060830
001800060830     d dsKey           ds
001900060830     d  dsaac                         4  0
002000060830     d  dsfil                         3  0
002100060830     d  dsnca                         7  0
002200060830      *
002300060405     d Dataiso         s               d   datfmt(*iso)
002400060405     d DataUltima      s               d   datfmt(*iso)
002500060830     d Datamese        s               d   datfmt(*iso)
002600051227      *-----------------------------------------------------------------*
002700061005     c                   time                    w0140            14 0
002800061005     c     *eur          move      w0140         dataultima
002900061005     c                   movel     dataultima    datacor           8 0
003000061005     c                   movel     dataultima    datamese
003100061005     c                   movel     w0140         wora              4 0
003200061005     c
003300061005     c     datamese      subdur    1:*m          datamese
003400061005     c                   movel     datamese      unmesefa          8 0
003500060405     c
003600051229
003700060830     c                   clear                   fidn12ds
003800061005     c                   eval      i12aas = 2005
003900061005     c                   eval      i12lnp = 006
004000061005     c                   eval      i12nrs = 00
004100061005     c                   eval      i12nsp = 949907
004200060830     c                   eval      i12fel = 'E'
004300060830     c                   eval      i12fan = 'E'
004400060830      *
004500060830     c                   call      'FIDN12R'
004600060830     c                   parm                    fidn12ds
004700060830      *
004800060830      * se non ci sono errori
004900060830     c                   if        o12err = ' '
005000060830      * se numero di CA trovate maggiore di zero
005100060830     c                   if        o12nca > 0
005200060830     c                   eval      EscludiCA='S'
005300060830     c
005400060830     c                   z-add     *zeros        ii                3 0
005500060830     c     1             do        o12nca        ii
005600060830     c                   movea     skkey(ii)     dskey
005700060830     C                   Z-ADD     skdca(ii)     dctdca            8 0
005800060830     C                   Z-ADD     skdch(ii)     dctdch            8 0
005900060830     c* Se chiusa --> elaboro
006000060830     c                   if        dctdch>0
006100060830     c                   clear                   escludiCA
006200060830     c                   leave
006300060830     c                   endif
006400060830     c*
006500060830     c                   if        dctdca>unmesefa
006600061005     c                   clear                   escludiCA         1
006700060830     c                   leave
006800060830     c                   endif
006900060830     C                   ENDDO
007000060830      *
007100060830     c                   endif
007200060830     c                   endif
007300060405     c
007400061005     c                   Eval      *InLr = *On
