000100981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*--------------------------------------------------------------*
000300160721     Ftisig18d  CF   E             WORKSTN
000400160531     ftabel00f  if   e           k disk
000500160531      *
000600160531     D kpjba         e DS
000700020624     D*
000800160623     d dataiso                         d
000900160623     d dataeur                         d   datfmt(*eur)
001000160531     d coman           s             99
001100160531     d luncmd          s             15  5
001200160623     d DigitN          c                   const('0123456789')
001300160531     D cmd1            S              1    DIM(99) CTDATA PERRCD(99)
001400011026
001500000000     C*---------------------------------------------------------------*
001600160531      *
001700160531     c                   do        *hival
001800160531     c                   exfmt     video1
001900160531     c                   clear                   $msg
002000160531     c                   setoff                                       99
002100160531     c   kc              leave
002200160531     c                   exsr      contr
002300160531     c   99              iter
002400160531     c                   if        *inkf
002500160531      *pulisce file di work
002600160531     c                   exsr      pulisci
002700160721      *richiama pgm per estrazione dati
002800160721     c                   movel     vanmes        kpjbu
002900160721     c                   call      'TISIG18R1'
003000160721     c                   parm                    kpjba
003100160531     c                   leave
003200160531     c                   endif
003300160531     c                   enddo
003400020624
003500160531     c                   seton                                        lr
003600160531      *______________________________________________________________
003700160531     c     contr         begsr
003800160531      *______________________________________________________________
003900160531     c                   if        vanno > annomx or vanno < annomi
004000160531     c                   seton                                        99
004100160531     c                   eval      $msg = 'Anno non corretto'
004200160531     c                   leavesr
004300160531     c                   endif
004400160721     c                   if        vmese > mesemx
004500160721     c                   seton                                        99
004600160721     c                   eval      $msg = 'Mese non corretto'
004700160721     c                   leavesr
004800160721     c                   endif
004900160531     c                   movel     vanno         vanmes            6 0
005000160531     c                   move      vmese         vanmes
005100160531     c                   endsr
005200160531      *______________________________________________________________
005300160531     c     pulisci       begsr
005400160531      *______________________________________________________________
005500160531     c                   movea     cmd1          coman
005600160531     C                   z-add     99            luncmd
005700160531     C                   CALL      'QCMDEXC'                            34
005800160531     C                   parm                    coman
005900160531     C                   parm                    luncmd
006000160531     c                   endsr
006100160531      *______________________________________________________________
006200160531     c     *inzsr        begsr
006300160531      *______________________________________________________________
006400160531     c     *entry        plist
006500160531     c                   parm                    kpjba
006600160531      *
006700160623     C                   TIME                    W0140            14 0
006800160623     C                   MOVE      W0140         oggi              8 0
006900160623     c                   move      oggi          dataeur
007000160623     c                   move      dataeur       dataiso
007100160623     c                   move      dataiso       oggig             8 0
007200160721     c                   extrct    dataiso:*m    vmese
007300160721     c                   sub       1             vmese
007400160721     c                   z-add     vmese         mesemx            2 0
007500160721     c                   extrct    dataiso:*y    vanno
007600160721     c                   z-add     vanno         annomx            4 0
007700160721     c     vanno         sub       1             annomi            4 0
007800160531      *
007900160531     c                   endsr
008000011129     C*--------------------------------------------------------------------
008100160531**  CMD1
008200160722CLRPFM    FILE(wfrtma0f)
