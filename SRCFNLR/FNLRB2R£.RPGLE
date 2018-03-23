000100040206     h decedit('0,') datedit(*DMY/) option(*nodebugio)
000200040206      * FNLRB2R *----------------------------------------------------*
000300040206      *         PULIZIA STORICO SPUNTE DA CML E DA VDL               *
000400040206      *--------------------------------------------------------------*
000500040206     fFLTR701L  uf   e           k disk
000600040206      *---------------------------------------------------------------*
000700040206     d KPJBA         e ds
000800040206     d PARAM           ds
000900040206     d  DatPul                 5     12  0
001000040206     d  DatPulA                5     12
001100040206      *
001200040206     d WLBdat          ds                  inz
001300040206     d  G02dat                 1      8  0 inz
001400040206     d  G02inv                 9     16  0 inz
001500040206     d  G02err                17     17    inz
001600040206     d  G02tgi                18     22  0 inz
001700040206      *
001800040206     d w0060           s              6  0 inz
001900040206      *---------------------------------------------------------------*
002000040206     c     *ENTRY        plist
002100040206     c                   parm                    KPJBA
002200040206     c                   movel     KPJBU         PARAM
002300040206      *----
002400040206      * STORICO SPUNTE      -VDL- (FLTR700F)
002500040206      *----
002600040206      *
002700040206      * Ciclo di lettura e di cancellazione record per P.O./Data Spunta
002800040206      *
002900040206     c                   clear                   TR7fil
003000040206do  1c                   DO        *hival
003100040206      *
003200040206      * - posizionamento iniziale per P.O.
003300040206     c     TR7fil        setgt     FLTR7000
003400040206     c                   read      FLTR7000
003500040206if  2c                   if            %eof(FLTR701L)
003600040206     c                   leave
003700040206e   2c                   endif
003800040206      *
003900040206      * - ciclo di cancellazione per P.O./Data Spunta
004000040206do  2c                   dow       NOT %eof(FLTR701L)
004100040206     c                             and %subst(TR7tim:1:8) <= DatPulA
004200040206      *
004300040206     c                   DELETE    FLTR7000
004400040206      *
004500040206     c     TR7fil        reade     FLTR7000
004600040206e   2c                   enddo
004700040206      *
004800040206e   1c                   ENDDO
004900040206      *
005000040206      * Fine
005100040206     c                   eval      *inLR = *on
