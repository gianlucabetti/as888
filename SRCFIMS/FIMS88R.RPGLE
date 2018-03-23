000100031007      *===============================================================*
000200080806      * FIMS88R * Creazione profili per pistole radio,pdaaut,vdl      *
000300031007      *===============================================================*
000400031014
000500031022     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600031007
000700031014      *---------------------------------------------------------------*
000701080807     FAZORG01L  IF   E           K DISK
000800080806     fFIMS88D   cf   e             workstn
000900031007
001000031007      *---------------------------------------------------------------*
001100031014
002000031007      * - Messaggi di errore
002100080807     d $Msg            s             78    dim(2) ctdata  perrcd(1)
002500031007      *
002600031007      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002700031007      *
002800031007     d KPJBA         e ds
002900080129      *
003000031007      * - Parametri x Controllo profilo utenti
003100080122     d TIBS34ds      e ds                  inz
003200031007      * - Ds di riferimento al file esterno AZUTE00F
003300080122     d AZUTEds       e ds                  extname(AZUTE00F)
003400031007      * - Ds per dati organigramma
003500080122     d dDatiUte      e ds
003501080807      * - Parametri per TNSD24R
003502080807     d §§Cod           s              3    inz
003503080807     d §§Tip           s                   like(ORGfag)  inz
003504080807     d §§Des           s             25    inz
003505080807     d §§Pra           s              3    inz
003506080807
003507080807     d Digits          c                   const('0123456789')
004500031007
004600031007      *---------------------------------------------------------------*
004700031007      *  RIEPILOGO INDICATORI                                         *
004800031007      *---------------------------------------------------------------*
004900031007      * 41    - P.O. in gestione errato                               *
005000031007      * 90    - Generico di errore                                    *
005100031007      *---------------------------------------------------------------*
005101080806     c     *entry        plist
005102080806     C                   parm                    kpjba
005103080806
005104080806      *
005105080806     c     *dtaara       define    §azute        azuteds
005106080806     c     *dtaara       define    §datiute      ddatiute
005107080806     c                   in(e)     *dtaara
005108080806     c                   if        %error  or rsut = *blanks
005109080806     c                   clear                   tibs34ds
005110080806     c                   call      'TIBS34R'
005111080806     c                   parm                    tibs34ds
005112080806     c                   in        *dtaara
005113080806     c                   endif
005114080806
005115080806     c     inizio        tag
005116080806     c                   clear                   v1cfil
005117080806     c                   clear                   v1dfil
005118080806     c                   clear                   v1cpr
005119080806     c                   clear                   v1caut
005120080806     c                   clear                   v1cvdl
005121080806     c                   do        *hival
005200080806     c                   write     ms88t01
005300080806     C                   exfmt     ms88d01
005301080806     c                   setoff                                       9028
005302080807     c                   clear                   v1dmsg
005303080806      * F3=Fine
005304080806     C                   if        *INKC
005305080806     C                   leave
005306080806     c                   endif
005307080806      * controlli videata
005308080806     C                   EXSR      Ctrd01
005309080806     c   90              iter
005310080806     c                   if        *inkf
005311080806     c                   exsr      lancio
005312080806     c                   goto      inizio
005313080806     c                   endif
005314080806     C                   ENDDO
005315080807     c                   eval      *inlr=*on
005316080806     c**********************************************************************
005317080806     C     ctrd01        begsr
005318080807     c                   setoff                                       4041
005319080807     c                   clear                   v1dfil
005320080806     c* controllo la filiale
005321080806     c                   if        v1cfil=*blanks or v1cfil=*zeros
005322080806     c                   seton                                        2840
005323080807     c                   eval      v1dmsg=$msg(1)
005325080806     c                   endif
005326080807     c*
005327080806     c                   if        %scan('?':v1cfil)>0
005328080807     C                   MOVE      'E'           §§Tip
005329080807     C                   CALL      'TNSD24R'
005330080807     C                   PARM                    §§Cod
005331080807     C                   PARM                    §§Tip
005332080807     C                   PARM                    §§DES
005333080807     C                   PARM                    §§PRA
005334080807     C                   MOVEL     §§COD         V1cfil
005335080807     C                   MOVEL     §§des         V1dfil
005336080806     c                   goto      e_ctrd01
005337080806     c                   endif
005338080807     C** CONTROLLO CHE IL CODICE FILIALE IMMESSO SIA
005339080807     C** NUMERICO
005344080807     c                   if        %check(digits:v1cfil)>0
005345080807     C                   SETON                                        289040
005346080807     C                   eval      v1dmsg=$msg(1)
005347080807     c                   goto      e_ctrd01
005348080807     C                   endif
005349080807
005350080807     c                   move      v1cfil        w0030             3 0
005351080806     c     w0030         chain     azorg01l
005352080807     c                   if        not %found(azorg01l) or
005353080807     c                             (orgfag <>'F' and orgfag<>'A') or
005354080807     c                             orgfva<>' '
005355080807     c                   seton                                        284090
005356080807     c                   eval      v1dmsg=$msg(1)
005357080807     c                   goto      e_ctrd01
005358080807     c                   endif
005359080807     C                   MOVEL     orgdes        V1dfil
005360080807
005361080807     c* tipo profilo - Selezionarne almeno uno
005362080807     c                   if        v1cpr=' ' and v1caut=' ' and
005363080807     c                             v1cvdl=' '
005364080807     c                   seton                                        284190
005365080807     c                   eval      v1dmsg=$msg(2)
005366080807     c                   goto      e_ctrd01
005367080807     c                   endif
005368080806     c
005369080806     C     e_ctrd01      endsr
005400031007
005401080807     c**********************************************************************
005402080807     C     lancio        begsr
005440080807     c
005442080807     c                   if        v1cpr='X' and v1caut='X' and
005443080807     c                             v1cvdl='X'
005445080807     c                   call      'FIMS88C'
005446080807     c                   parm                    v1cfil
005447080807     c                   parm      'X'           tipo
005448081218     c                   parm                    v1catp
005449081218     c                   parm                    v1cata
005450090318     c                   parm                    v1catv
005451080807     c                   else
005452080807     c                   if        v1cpr='X'
005453080807     c                   call      'FIMS88C'
005454080807     c                   parm                    v1cfil
005455080807     c                   parm      'P'           tipo
005456081218     c                   parm                    v1catp
005457081218     c                   parm                    v1cata
005458090318     c                   parm                    v1catv
005459080807     c                   endif
005460080807     c                   if        v1cAUT='X'
005461080807     c                   call      'FIMS88C'
005462080807     c                   parm                    v1cfil
005463080807     c                   parm      'A'           tipo
005464081218     c                   parm                    v1catp
005465081218     c                   parm                    v1cata
005466090318     c                   parm                    v1catv
005467080807     c                   endif
005468080807     c                   if        v1cVDL='X'
005469080807     c                   call      'FIMS88C'
005470080807     c                   parm                    v1cfil
005471080807     c                   parm      'V'           tipo              1
005472081218     c                   parm                    v1catp
005473081218     c                   parm                    v1cata
005474090318     c                   parm                    v1catv
005475080807     c                   endif
005476080807     c                   endif
005477080807     C                   endsr
005500031007
005600031007** - $MSG
005700080807Filiale errata o inesistente                                                      1
005800080807Selezionare almeno un tipo di profilo                                             1
