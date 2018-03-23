000100990614     h DECEDIT('0,') DATEDIT(*DMY.)
000200980323     H*------------------------------------------------------------------------*
000300031103     H* PREP. FILE WORK WFAUT00F PER AUMENTO TARIFFE 2004                      *
000400031103     H*                                                                        *
000500031103     H*                                                                        *
000600980323     H*------------------------------------------------------------------------*
000700980323     F*
000800031103     FWfdelpcB  if   e             DISK
000900031103     FWfaut01l  uf   e           k DISK
001000990614      *
001100031103
001200031103      *------------------------------------------------------------------------*
001300031103
001400031103     D Tipocl          S                   like(WFAtcl) INZ('U')
001500031103
001600031103
001700031103      *------------------------------------------------------------------------*
001800031103
001900031103      *
002000031103     c     keyaut        klist
002100031103     c                   kfld                    wfpage
002200031103     c                   kfld                    tipocl
002300031103     c                   kfld                    wfpcli
002400031103      *
002500031103     c                   do        *hival
002600031103      *
002700031103     c                   read      WFDELPCB
002800031103      *
002900031103     c                   if        %eof(WFDELPCB)
003000031103     c                   leave
003100031103     c                   endif
003200031103      *
003300031103      * Per ogni record vado a cercare il corrispondente cliente unificante
003400031103      *
003500031103     c     keyaut        chain     Wfaut01l
003600031103      *
003700031103     c                   if        not %found(Wfaut01l)
003800031103     c                   iter
003900031103     c                   endif
004000031103      * aggiorno
004100031103     c                   eval      Wfasac = Wfpsac
004200031103     c                   eval      Wfafac = Wfpfac
004300031103     c                   eval      Wfadac = Wfpdac
004400051102      * se numero spediizoni sono uguali a zero e il flag tipo tabulato uguale a 4
004500051102     c                   if        wfasac = 0 and  wfaftb = '4'
004600031113     c                   eval      wfaftb = 'X'
004700031113      * escludo il record dalla stampa del tabulato  (richiesta di Bocchi)
004800031113     c                   endif
004900031113      *
005000031113     c                   update    wfaut000
005100031103      *
005200031103     c                   enddo
005300031103      *
005400031103     c                   seton                                        lr
005500031103      *
