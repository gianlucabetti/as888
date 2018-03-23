000100020412     H DECEDIT('0,') DATEDIT(*ymd.) option(*nodebugio)
000200020529      *
000300060920      * TRULDEPR*---------------------------------------------------*
000400060920      * - Reperimento cod.cliente di un depot dpd
000500060920      *--------------------------------------------------------------*
000600020412
000700060920     fdppdc01l  if   e           k disk
000800020412
000900060920     d truldepds     e ds
001000060921     d wdata           s                   like(idepdrf)
001100020529
001200020412      *---------------------------------------------------------------
001300020412     c     *entry        plist
001400060920     c                   parm                    truldepds
001500020529
001600060920     c                   eval      odepksc = 0
001700060921     c                   eval      odeperr = '1'
001800060920     c                   eval      odeporma= *blanks
001900060921     c*
002000060921     c     kpdc          klist
002100060921     c                   kfld                    idepdpc
002200060921     c                   kfld                    wdata
002300020412
002400060920      * ideptla = ' '  Elaboro e chiudo con RETURN
002500060920      * ideptla = 'L'  Elaboro e chiudo con LR
002600060920      * ideptla = 'C'            chiudo con LR  solo per chiusura file
002700020412
002800060920    1c                   if        ideptla = 'C'
002900020412     c                   eval      *inlr = *on
003000020412
003100020412   X1c                   else
003200020808
003300020412
003400020412      * se non passata data dal chiamante prendo udate per i controlli della data
003500060920     c                   if        idepdrf <> *zeros
003600060920     c                   eval      wdata = idepdrf
003700020412     c                   else
003800020412     c                   eval      wdata = *date
003900020412     c                   endif
004000060921     c     kpdc          setll     dppdc01l
004001151112     c     idepdpc       readpe    dppdc01l
004100060921     c                   do        *hival
004300060921     c                   if        %eof(dppdc01l)
004400060921     c                   leave
004500060921     c                   endif
004600060921     c                   if        wdata >= pdcdde and wdata <= pdcdsc
004700060921     c                   clear                   odeperr
004800060921     c                   movel     pdcorma       odeporma
004900060921     c                   z-add     pdcksc        odepksc
005000060921     c                   leave
005100060921     c                   endif
005101151112     c     idepdpc       reade     dppdc01l
005200060921     c                   enddo
005300060920     c
005400060920    2c                   if        ideptla = 'L'
005500020412     c                   eval      *inlr = *on
005600020412   X2c                   else
005700020412     c                   return
005800020412    2c                   endif
005900020412
006000020412     c                   endif
006100020412
