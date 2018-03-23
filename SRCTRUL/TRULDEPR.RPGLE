000100020412     H DECEDIT('0,') DATEDIT(*ymd.) option(*nodebugio)
000200020529      *
000300060920      * TRULDEPR*---------------------------------------------------*
000400060920      * - Reperimento cod.cliente di un depot dpd
000500060920      *--------------------------------------------------------------*
000600170929     fdpleg11l  if   e           k disk    rename(dpleg000:dpleg11)
000700170929     fdpleg14l  if   e           k disk    rename(dpleg000:dpleg14)
000800060920     fdppdc01l  if   e           k disk
000900171002      *---------------------------------------------------------------
001000060920     d truldepds     e ds
001100171002     d truldepds1    e ds
001200060921     d wdata           s                   like(idepdrf)
001300171002      *---------------------------------------------------------------
001400020412     c     *entry        plist
001500060920     c                   parm                    truldepds
001600171002     c                   parm                    truldepds1
001700171002     c*
001800171002     c     kpdc          klist
001900171002     c                   kfld                    idepdpc
002000171002     c                   kfld                    wdata
002100020529
002200171031     c                   eval      *inlr = *on
002300060920     c                   eval      odepksc = 0
002400060921     c                   eval      odeperr = '1'
002500060920     c                   eval      odeporma= *blanks
002600020412
002700171031      * CHIUDE SEMPRE in LR qualsiasi parametro di TLA riceva
002800060920      * ideptla = 'L'  Elaboro e chiudo con LR
002900171031      * ideptla = 'C'            chiudo con LR
003000060920    1c                   if        ideptla = 'C'
003100171031   X1c                   RETURN
003200171031   X1c                   end
003300171031      **
003400171031     c                   if        %PARMS = 1 and
003500171031     c                             (iDEPdpc ='0000' or iDEPdpc ='    ')
003600171031   X1c                   RETURN
003700171031   X1c                   end
003800171031      **
003900171031     c                   if        %PARMS = 2 and
004000171031     c                             (iDEPdep7='0000000' or iDEPdep7=*blank) and
004100171031     c                             (iDEPdpc ='0000' or iDEPdpc ='    ')
004200171031   X1c                   RETURN
004300171031   X1c                   end
004400171031      ** ---------------------------------------------------------------------------
004500171031      **  SE non è uscito per problemi nei parametri  con il DEPOT
004600171031      ** ---------------------------------------------------------------------------
004700020412      * se non passata data dal chiamante prendo udate per i controlli della data
004800060920     c                   if        idepdrf <> *zeros
004900060920     c                   eval      wdata = idepdrf
005000020412     c                   else
005100020412     c                   eval      wdata = *date
005200171031     c                   end
005300170929      *--
005400171031     c                   eval       idepdrf = wdata
005500171031      ** ---------------------------------------------------------------------------
005600171031      *  Prima prova con il depot di 7 sui nuovi legami altrimenti prova con il
005700171031      *  depot di 4 prima sul vecchio legame e se scaduto tenta sul nuovo legame.
005800171031      ** ---------------------------------------------------------------------------
005900171031     c                   if          %Parms > 1
006000171102     c                   if        iDEPdep7 <> *blank and iDEPdep7 <> '0000000'
006100170929      ****   Depot (7)
006200170929     c                   movel     iDEPdep7      legDep7
006300171002     c                   clear                   iDEPflr1
006400170929     c     legDEP7       setll     dpleg11l
006500170929     c     legDEP7       reade     dpleg11l
006600170929      * non esiste ANAGRAFICA su CAPPARIO DPD
006700170929     c                   DOw       not %EoF(dpleg11l)
006800170929     c                   if        wdata >= LEGdde and wdata <= LEGdsc
006900170929     c                   clear                   odeperr
007000171002     c                   movel     legorma       odeporma
007100171002     c                   z-add     legksc        odepksc
007200171031     c                   RETURN
007300171031      *                *--------*
007400170929     c                   end
007500170929     c     legDEP7       reade     dpleg11l
007600170929     c                   endDO
007700171002      ****-------
007800171031     c                   RETURN
007900171031      *                *--------*
008000171002     c                   end
008100171031     c                   end
008200170929      ****-------
008300170929
008400170929      ****   Depot (4)  prima prova sul vecchio
008500170929     c     idepdpc       setll     dppdc01l
008600170929     c     idepdpc       reade     dppdc01l
008700170929     c                   dow       not %EoF(dppdc01l)
008800060921     c                   if        wdata >= pdcdde and wdata <= pdcdsc
008900060921     c                   clear                   odeperr
009000060921     c                   movel     pdcorma       odeporma
009100060921     c                   z-add     pdcksc        odepksc
009200171031     c                   RETURN
009300171031      *                *--------*
009400060921     c                   endif
009500151112     c     idepdpc       reade     dppdc01l
009600060921     c                   enddo
009700170929
009800170929      ****   Depot (4)  poi se non trova prova sul nuovo
009900170929      *--
010000170929     c                   movel     IDEPDPC       legDPC
010100170929     c     legDPC        setll     dpleg14l
010200170929     c     legDPC        reade     dpleg14l
010300170929      * non esiste ANAGRAFICA su CAPPARIO DPD
010400170929     c                   DOw       not %EoF(dpleg14l)
010500170929     c                   if        wdata >= LEGdde and wdata <= LEGdsc
010600170929     c                   clear                   odeperr
010700171019     c                   movel     legorma       odeporma
010800171019     c                   z-add     legksc        odepksc
010900171031     c                   RETURN
011000171031      *                *--------*
011100170929     c                   end
011200170929     c     legDPC        reade     dpleg14l
011300170929     c                   endDO
011400170929      ****-------
011500170929     c
011600171031     c                   RETURN
011700171031      *                *--------*
