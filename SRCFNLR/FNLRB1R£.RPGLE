000100040206     h decedit('0,') datedit(*DMY/) option(*nodebugio)
000200040206      * FNLRB2R *----------------------------------------------------*
000300040206      *         PULIZIA STORICO SPUNTE DA CML E DA VDL               *
000400040206      *--------------------------------------------------------------*
000500040206     fFLTR160F  uf   e           k disk
000600040206     fFLTR701L  uf   e           k disk
000700040206      *---------------------------------------------------------------*
000800040206     d KPJBA         e ds
000900040206     d PARAM           ds
001000040206     d  DatPul                 5     12  0
001100040206     d  DatPulA                5     12
001200040206      *
001300040206     d WLBdat          ds                  inz
001400040206     d  G02dat                 1      8  0 inz
001500040206     d  G02inv                 9     16  0 inz
001600040206     d  G02err                17     17    inz
001700040206     d  G02tgi                18     22  0 inz
001800040206      *
001900040206     d w0060           s              6  0 inz
002000040206      *---------------------------------------------------------------*
002100040206     c     *ENTRY        plist
002200040206     c                   parm                    KPJBA
002300040206     c                   movel     KPJBU         PARAM
002400040206      *----
002500040206      * SEGNACOLLI SPUNTATI -CML- (FLTR160F)
002600040206      *----
002700040206      *
002800040206      * Ciclo di lettura e di cancellazione record per Data Spunta
002900040206      *
003000040206     c                   read      FLTR160
003100040206      *
003200040206do  1c                   DOW       NOT %eof(FLTR160F)
003300040206     c                   clear                   WLBdat
003400040206     c                   movel     TR1tim        w0060
003500040206     c                   z-add     w0060         G02dat
003600040206     c                   call      'XSRDA8'
003700040206     c                   parm                    WLBdat
003800040206      *
003900040206if  2c                   if        G02inv <= DatPul
004000040206     c                   DELETE    FLTR160
004100040206e   2c                   endif
004200040206      *
004300040206     c                   read      FLTR160
004400040206e   1c                   ENDDO
004500040206      *----
004600040206      * STORICO SPUNTE      -VDL- (FLTR700F)
004700040206      *----
004800040206      *
004900040206      * Ciclo di lettura e di cancellazione record per P.O./Data Spunta
005000040206      *
005100040206     c                   clear                   TR7fil
005200040206do  1c                   DO        *hival
005300040206      *
005400040206      * - posizionamento iniziale per P.O.
005500040206     c     TR7fil        setgt     FLTR7000
005600040206     c                   read      FLTR7000
005700040206if  2c                   if            %eof(FLTR701L)
005800040206     c                   leave
005900040206e   2c                   endif
006000040206      *
006100040206      * - ciclo di cancellazione per P.O./Data Spunta
006200040206do  2c                   dow       NOT %eof(FLTR701L)
006300040206     c                             and %subst(TR7tim:1:8) <= DatPulA
006400040206      *
006500040206     c                   DELETE    FLTR7000
006600040206      *
006700040206     c     TR7fil        reade     FLTR7000
006800040206e   2c                   enddo
006900040206      *
007000040206e   1c                   ENDDO
007100040206      *
007200040206      * Fine
007300040206     c                   eval      *inLR = *on
