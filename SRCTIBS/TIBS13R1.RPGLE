000100160607     H DEBUG DECEDIT('0,') DATEDIT(*ymd/)
000200160614     ftikun01l  uf   e           k disk    infds(inftikun)
000300160607     fazorg01l  if   e           k disk
000400021022     fqsysprt   o    f  132        printer infds(stamp)
000500160607     d og143         e ds
000600160607     D inftikun        ds
000700160607     D  rrn                  397    400b 0
000800021022     d stamp           ds
000900021022     d  line                 367    368b 0
001000160607     D                 DS
001100160607     D  FILCLI                 5      7  0
001200160607     D  STRATECLI              1     11
001300160607     d ntwp            s              3
001400160607     d ntwf            s              3
001500160607     d dataeur         s               d   datfmt(*eur)
001600160607     d dataiso         s               d
001700160607     d dataday         s               d
001800000607      *--------------------------------------------------------------------
001900000607     c
002000160607     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
002100160607     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
002200160607     c                   move      wn8           dataeur
002300160607     c                   move      dataeur       dataday
002400021022     c                   z-add     99            line
002500021022
002600160607     c     *start        setll     tikun01l
002700000607     c                   do        *hival
002800160607     c                   read      tikun01l                               99
002900020904     c   99              leave
003000160607      *attivo
003100160607     c                   if        kunvat <> *blank or
003200160607     c                             kuntle <> 'ST'
003300160607     c                   iter
003400160607     c                   endif
003500160607      *scaduto
003600160607     c                   move      kundsc        dataiso
003700160607     c                   if        dataiso < dataday
003800160607     c                   iter
003900160607     c                   endif
004000160607      * ntw padre
004100160607     c                   clear                   ntwp
004200160607     C                   move(p)   kuncop        STRATECLI
004300160607     c     filcli        chain     azorg01l
004400160607     c                   if        %found(azorg01l)
004500160607     c                   movel     orgde3        og143
004600160607     c                   move      §ogntw        ntwp
004700160607     c                   endif
004800160607      * ntw figlio
004900160607     c                   clear                   ntwf
005000160607     C                   move(p)   kuncof        STRATECLI
005100160607     c     filcli        chain     azorg01l
005200160607     c                   if        %found(azorg01l)
005300160607     c                   movel     orgde3        og143
005400160607     c                   move      §ogntw        ntwf
005500160607     c                   endif
005600160607      * controllo il tipo legame sulla base del network
005700160607      * per tipo legame ST il ntw padre figlio devono essere dello stesso tipo
005800160607     c                   if        ntwp = 'LOG' and ntwf <> 'LOG' or
005900160607     c                             ntwp <> 'LOG' and ntwf = 'LOG'
006000160614     c                   if        line > 60
006100160614     c                   except    testa
006200160614     c                   endif
006300160614     c                   delete    tikun000
006400160607     c                   except    riga
006500160607     c                   endif
006600160607     c*
006700021022
006800000607     c                   enddo
006900000607     c                   seton                                        lr
007000021022     O*------------------------------------------------------*
007100021022     OQSYSPRT   e            TESTA             2
007200160607     O                                           70 'Elenco famiglie miste nel -
007300160607     O                                              file TIKUN00F'
007400160607     O          e            testa       1
007500160607     o                                          + 1 'Nrr rekord Tikun'
007600160607     o                                          + 1 'legame'
007700160607     o                                          + 1 'Riferimenti padre'
007800160607     o                                          + 1 'Riferimenti figlio'
007900021022
008000160607     O          e            riga        1
008100160607     o                       rrn           z    + 1
008200160607     o                       kuntle              23
008300160607     o                       kuncop        z    + 3
008400160607     o                       ntwp               + 1
008500160607     o                       kuncof        z    + 3
008600160607     o                       ntwf               + 1
