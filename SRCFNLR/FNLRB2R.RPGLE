000100130530     /*PRM  dbgview(*source)
000200130530     /*END
000300040206     h decedit('0,') datedit(*DMY/) option(*nodebugio)
000400040206      * FNLRB2R *----------------------------------------------------*
000500040206      *         PULIZIA STORICO SPUNTE DA CML E DA VDL               *
000600040206      *--------------------------------------------------------------*
000700130530     fFLTR702L  uf   e           k disk
000800040206      *---------------------------------------------------------------*
000900040206     d KPJBA         e ds
001000040206     d PARAM           ds
001100040206     d  DatPul                 5     12  0
001200040206     d  DatPulA                5     12
001300040206      *
001400040206     d WLBdat          ds                  inz
001500040206     d  G02dat                 1      8  0 inz
001600040206     d  G02inv                 9     16  0 inz
001700040206     d  G02err                17     17    inz
001800040206     d  G02tgi                18     22  0 inz
001900040206      *
002000040206     d w0060           s              6  0 inz
002100040206      *---------------------------------------------------------------*
002200040206     c     *ENTRY        plist
002300040206     c                   parm                    KPJBA
002400040206     c                   movel     KPJBU         PARAM
002500040206      *----
002600040206      * STORICO SPUNTE      -VDL- (FLTR700F)
002700040206      *----
002800040206      *
002900040206      * Ciclo di lettura e di cancellazione record per P.O./Data Spunta
003000040206      *
003100040206     c                   clear                   TR7fil
003200040206do  1c                   DO        *hival
003300040206      *
003400040206      * - posizionamento iniziale per P.O.
003500040206     c     TR7fil        setgt     FLTR7000
003600040206     c                   read      FLTR7000
003700130530if  2c                   if            %eof(FLTR702L)
003800040206     c                   leave
003900040206e   2c                   endif
004000040206      *
004100040206      * - ciclo di cancellazione per P.O./Data Spunta
004200130530do  2c                   dow       NOT %eof(FLTR702L)
004300040206     c                             and %subst(TR7tim:1:8) <= DatPulA
004400040206      *
004500040206     c                   DELETE    FLTR7000
004600040206      *
004700040206     c     TR7fil        reade     FLTR7000
004800040206e   2c                   enddo
004900040206      *
005000040206e   1c                   ENDDO
005100040206      *
005200040206      * Fine
005300040206     c                   eval      *inLR = *on
