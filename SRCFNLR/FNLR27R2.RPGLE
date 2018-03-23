000100050510     H DECEDIT('0,') DATEDIT(*yMd.)
000200111109     H* FNLR27R2 *-----------------------------------------------------*
000300930505     H*         - ABILITAZIONE BOLLE ARRIVI ALLA CONSEGNA -           *
000400111109     h*      controlla trasmissione pesi e volumi gni 2 ore
000500930505     H*---------------------------------------------------------------*
000600040505     d minuti          s              5  0
000700040505     d worap           s               z
000800040505     d worad           s               z
000900111109     C*---------------------------------------------------------------*
001000111109     C     *ENTRY        PLIST
001100111109     C                   PARM                    Sitrasm           1
001200111109     c
001300111114     c* Sottometto trasmissione :
001400111114     c* 1) Ogni due ore e all'apertura del pgm o del sottosistema
001500111114     c* 2) All'apertura del pgm o del sottosistema
001600111114     c*    in questo secondo caso mi viene passata una "S" dal clp
001700111114     c
001800111114     c                   if        Sitrasm<>'S'
001900111006     c                   clear                   SItrasm           1
002000040505     c
002100111109     c* Se sono passate 2 ore passo flag per trasmettere pesi e volumi
002200040505     c                   time                    worad
002300040505     c     worad         subdur    worap         minuti:*MN
002400111114    3c                   if        minuti>120
002500040505     C                   SETON                                        LR
002600111006     c                   eval      SItrasm='S'
002700041104   x3c                   else
002800111109     C                   SETON                                        RT
002900041104    3c                   endif
003000111114     c
003100111114     c                   else
003200111114     C                   SETON                                        RT
003300111114    3c                   endif
003400111109    2c
003500930430     C*---------------------------------------------------------------*
003600930430     C*                    DEFVAR                                     *
003700930430     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
003800930430     C*---------------------------------------------------------------*
003900040401     C     *inzsr        BEGSR
004000040505     c* Prendo ora di apertura pgm
004100040505     C                   time                    worap
004200111109     c                   ENDSR
