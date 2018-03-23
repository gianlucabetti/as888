000100050722      *PARMS OPTION(*NOXREF) TGTRLS(*CURRENT)
000200050722      *===============================================================*
000300050722      *?FNLS68R    * Annullamento dettaglio DISK CLI senza testate   ?*
000400050722      *===============================================================*
000500050722
000600050722     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700050722
000800050722     fFNLS68D   cf   e             workstn
000900050722
001000050831     d $Msg            s             80    dim(01) ctdata perrcd(1)
001100050831      *
001200050722     d KPJBA         e ds
001300050722      *
001400050722      * - Parametri x Controllo profilo utenti
001500050722     d TIBS34ds      e ds
001600050722      * - Ds di riferimento al file esterno AZUTE00F
001700050722     d AZUTEds       e ds                  extname(AZUTE00F)
001800050722      * - Ds per dati organigramma
001900050722     d dDATIUTE      e ds
002000050722      *
002100050722     d Status         sds           333
002200050722     d   SDSpgm          *proc
002300050722
002400050722     c     *Entry        plist
002500050722     c                   parm                    KPJBA
002600050722      *
002700050722      * Reperisco nome azienda per stampa PREPAGATI
002800050722     c     *dtaara       define    §azute        AZUTEds
002900050722     c     *dtaara       define    §datiute      dDATIUTE
003000050722     c                   in(E)     *dtaara
003100050722     c                   if        %ERROR or RSUT = *blanks
003200050722     c                   clear                   Tibs34Ds
003300050722     c                   call      'TIBS34R'
003400050722     c                   parm                    Tibs34Ds
003500050722     c                   in        *dtaara
003600050722     c                   endif
003700050722      *
003800050722     c                   movel     SDSpgm        V1Tpgm
003900050722      *
004000050722      * Emissione videata
004100050831do  1c                   dou           *inKC
004200050831     c                             or (*inKF and not *in90)
004300050722     c                   exfmt     LS68D01
004400050831     c                   exsr      CtrD01
004500050722e   1c                   enddo
004600050722      *
004700050722      * Richiamo pgm. batch
004800050831if  1c                   if        *inKF and not *in90
004900050722     c                   movel(p)  V1Cccm        KPJBU
005000050722     c                   eval      Kcoaz   = 'LS67'
005100050722     c***                movel(p)  'FNLS67R'     Wpgm             10
005200050722     c                   movel(p)  'BCH10'       Wpgm             10
005300050722     c                   call      Wpgm
005400050722     c                   parm                    KPJBA
005500050722e   1c                   endif
005600050722      *
005700050722     c                   eval      *inLR   = *on
005800050831      *
005900050831      *---------------------------------------------------------------*
006000050831      *
006100050831     c     CtrD01        BEGSR
006200050831      *
006300050831     c                   clear                   V1Dmsg
006400050831     c                   setoff                                       28  90
006500050831     c                   movea     *zeros        *in(41)
006600050831      *
006700050831      * Codice cliente obbligatorio
006800050831     c                   if        V1Cccm <= *zeros
006900050831     c                   seton                                        284190
007000050831     c                   eval      V1Dmsg  = $Msg(1)
007100050831     c                   leavesr
007200050831     c                   endif
007300050831      *
007400050831     c                   ENDSR
007500050831
007600050831**   $Msg
007700050831Immettere il codice cliente
