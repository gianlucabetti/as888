000100050722      *PARMS OPTION(*NOXREF) TGTRLS(*CURRENT)
000200050722      *===============================================================*
000300050722      *?FNLS68R    * Annullamento dettaglio DISK CLI senza testate   ?*
000400050722      *===============================================================*
000500050722
000600050722     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700050722
000800050902      *---------------------------------------------------------------*
000900050902
001000050722     fFNLS68D   cf   e             workstn
001100050902     fAZORG01L  if   e           k disk
001200050722
001300050902      *---------------------------------------------------------------*
001400050902
001500050905     d $Msg            s             80    dim(05) ctdata perrcd(1)
001600050831      *
001700050722     d KPJBA         e ds
001800050722      *
001900050722      * - Parametri x Controllo profilo utenti
002000050722     d TIBS34ds      e ds
002100050722      * - Ds di riferimento al file esterno AZUTE00F
002200050722     d AZUTEds       e ds                  extname(AZUTE00F)
002300050722      * - Ds per dati organigramma
002400050722     d dDATIUTE      e ds
002500050722      *
002600050722     d Status         sds           333
002700050722     d   SDSpgm          *proc
002800050902      *
002900050905     d FNLS67ds        ds                  inz
003000050902     d  V1Cccm                             inz
003100050902     d  V1Clnp                             inz
003200050902     d  V1Cnrs                             inz
003300050722
003400050902      *---------------------------------------------------------------*
003500050902
003600050722     c     *Entry        plist
003700050722     c                   parm                    KPJBA
003800050902      *
003900050902      * Operazioni iniziali
004000050902     c                   exsr      OpeIni
004100050722      *
004200050902      * Gestione videata
004300050902do  1c                   do        *hival
004400050902      *
004500050902      * - emissione
004600050722     c                   exfmt     LS68D01
004700050902      * - F3=Fine
004800050902if  2c                   if        *inKC
004900050902     c                   leave
005000050902e   2c                   endif
005100050902      * - controllo dati
005200050831     c                   exsr      CtrD01
005300050902      * - F6=Elaborazione
005400050902if  2c                   if        *inKF and not *in90
005500050902     c                   exsr      sr_SbmJob
005600050902     c                   leave
005700050902e   2c                   endif
005800050902      *
005900050722e   1c                   enddo
006000050722      *
006100050902      * Fine
006200050722     c                   eval      *inLR   = *on
006300050902      *
006400050902      *---------------------------------------------------------------*
006500050902      *
006600050902     c     OpeIni        BEGSR
006700050902      *
006800050902      * Reperisco nome azienda per stampa PREPAGATI
006900050902     c     *dtaara       define    §azute        AZUTEds
007000050902     c     *dtaara       define    §datiute      dDATIUTE
007100050902     c                   in(E)     *dtaara
007200050902     c                   if        %ERROR or RSUT = *blanks
007300050902     c                   clear                   Tibs34Ds
007400050902     c                   call      'TIBS34R'
007500050902     c                   parm                    Tibs34Ds
007600050902     c                   in        *dtaara
007700050902     c                   endif
007800050902      *
007900050902     c                   movel     SDSpgm        V1Tpgm
008000050902      *
008100050902     c                   ENDSR
008200050831      *
008300050831      *---------------------------------------------------------------*
008400050831      *
008500050831     c     CtrD01        BEGSR
008600050831      *
008700050831     c                   clear                   V1Dmsg
008800050831     c                   setoff                                       28  90
008900050831     c                   movea     *zeros        *in(41)
009000050902      *
009100050902      * Non immesso alcun dato
009200050902     c                   if             V1Cccm =  *zeros
009300050905     c                             and  V1Clnp =  *zeros
009400050905     c                             and  V1Cnrs =  *zeros
009500050902     c                   seton                                        412890
009600050902     c                   eval      V1Dmsg  = $Msg(1)
009700050905     c                   leavesr
009800050902     c                   endif
009900050902      * Immessi troppi dati
010000050902     c                   if             V1Cccm <> *zeros
010100050902     c                             and (V1Clnp <> *zeros
010200050902     c                              or  V1Cnrs <> *zeros)
010300050902     c                   seton                                        412890
010400050902     c                   eval      V1Dmsg  = $Msg(2)
010500050905     c                   leavesr
010600050902     c                   endif
010700050902      * Immessi dati incompleti
010800050902     c                   if             V1Clnp  = *zeros
010900050902     c                             and  V1Cnrs <> *zeros
011000050902     c                   seton                                        422890
011100050902     c                   eval      V1Dmsg  = $Msg(3)
011200050905     c                   leavesr
011300050902     c                   endif
011400050902     c                   if             V1Clnp <> *zeros
011500050902     c                             and  V1Cnrs  = *zeros
011600050902     c                   seton                                        432890
011700050902     c                   eval      V1Dmsg  = $Msg(4)
011800050905     c                   leavesr
011900050902     c                   endif
012000050905      *
012100050905      * Controllo codice P.O.
012200050905     c                   clear                   V1Dlnp
012300050905     c                   if        V1Clnp  > *zeros
012400050905     c     V1Clnp        chain     AZORG
012500050905     c                   if             %found(AZORG01L)
012600050905     c                             and  ORGfva  = *blanks
012700050905     c                   movel     ORGdes        V1Dlnp
012800050905     c                   else
012900050905     c                   seton                                        422890
013000050905     c                   eval      V1Dmsg  = $Msg(5)
013100050905     c                   leavesr
013200050905     c                   endif
013300050905     c                   endif
013400050905      *
013500050831     c                   ENDSR
013600050902      *
013700050902      *---------------------------------------------------------------*
013800050902      *
013900050902     c     sr_SbmJob     BEGSR
014000050902      *
014100050902      * Richiamo pgm. batch
014200050902     c                   eval      Kcoaz   = 'LS67'
014300050905     c                   movel(p)  FNLS67ds      KPJBU
014400050902      *
014500050902     c***                movel(p)  'FNLS67R'     Wpgm             10
014600050902     c                   movel(p)  'BCH10'       Wpgm             10
014700050902     c                   call      Wpgm
014800050902     c                   parm                    KPJBA
014900050902      *
015000050902     c                   ENDSR
015100050831
015200050831**   $Msg
015300050906Effettuare una selezione (un codice cliente o una linea di partenza e serie)
015400050906Immettere il codice cliente   OPPURE   la linea di partenza e la serie
015500050906Immettere anche la linea di partenza (oltre alla serie)
015600050906Immettere anche la serie (oltre alla linea di partenza)
015700050906Linea di partenza errata
