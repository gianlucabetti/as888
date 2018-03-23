000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200000000     H*--------------------------------------------------------------*
000300120531     Faiats00F  uF   E           K DISK
000400111206     F                                     commit
000500120531     C*------------------------------------------------------------------------*
000600120531     D KPJBA         E DS
000700120531     D trul33ds      E DS
000800111123     C*------------------------------------------------------------------------*
000900111123     C*        - OPERAZIONI INIZIALI
001000111123     C*------------------------------------------------------------------------*
001100111123     C     *ENTRY        PLIST
001200111123     C                   PARM                    AGGIORNA          1
001300120531     C                   PARM                    ESITO_ALF         1
001400111123     C*
001500111123     C     AGGIORNA      comp      ' '                                    44
001600090423     C*=================================================================
001700120531     C                   DO        *hival
001800120531
001900120531     c                   read      aiats00f
002000111122      *  EoF
002100120531     C                   if        %Eof(aiats00f)
002200081110     c                   Leave
002300081110     C                   ENDif
002400120531      *  Annullato
002500120531     C                   if        atsATB <> *blank
002600120531     c                   iter
002700120531     C                   ENDif
002800120531      ****
002900120531     C* Num => Stacco un numeratore da AZNUM  (37)
003000120531      ****
003100120604      **   Non deve essere un BIS ma deve essere  un trazionista
003200120604      **   Quindi la data Decorrenza contratto > 0
003300120605      ****
003400120604     C                   if        aggiorna <> *blank
003500120604     C                   if        atsNRC = *zeros   and
003600120604     C                             atsDEC > *zeros
003700120531      **
003800120531     c                   clear                   kpjbu
003900120531     C                   clear                   TRUL33DS
004000120531     C                   eval      I33OPE = *zeros
004100120531     C                   eval      I33CNU = 37
004200120531     C                   eval      I33NUM = 1
004300120531     C                   movel     TRUL33DS      KPJBU
004400120531     C                   call      'TRUL33R'
004500120531     C                   parm                    KPJBA
004600120531     C                   movel     KPJBU         TRUL33DS
004700120531      ****
004800120531     C                   if        O33ERR = *zeros
004900120531     c                   z-add     o33nrf        atsNRC
005000120531     C                   else
005100120531     c                   z-add     9999999       atsNRC
005200120604     C                   move      'E'           ESITO_ALF         1
005300120531     C                   endif
005400120531      **
005500120604      ** imposta la data fine contratto "ad libitum"
005600120604     c                   if        atsdfc = 0
005700120604     c                   z-add     20391231      atsdfc
005800120604     C                   endif
005900120604     C                   endif
006000120604      **
006100111205      ****
006200120531     c                   update    aiATS000
006300120604     c                   end
006400120604      ****
006500081110     C                   ENDDO
006600081110     C*
006700081110     C     FINE          TAG
006800120531     c                   COMMIT
006900120531      ***
007000120531     C                   SETON                                        LR
007100081110     c                   return
007200090424     C*---------------------------------------------------------------*
