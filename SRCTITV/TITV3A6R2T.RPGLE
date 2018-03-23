000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200131004     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300991027
000400161129
000500161129
000600041015     D*-----------
000700041015     D* VARIABILI D WRK
000800041015     D*-----------
000900170912     D wObj            s           1000    inz varying
001000171002     D iDataEvento     s              8s 0
001100171002     D iOraEvento      s              6s 0
001200131004
001300170912
001400171002     D iAmbiente       s              1a
001500171002     D iSAPnumDOC      s             10a
001600171002     D iStatus         s              4a
001700171002     D iTipoStato      s              2a
001800170912     D iStatusDate     s              8a   inz
001900170912     D iStatusTime     s              6a   inz
002000170912     D oReturnCode     s             10a   inz
002100170912     D oReturnDescr    s             80a   inz
002200131004
002300030924
002400170912
002500170912     C* MAIN/
002600170912     C*
002700041015     C* Esegue traduzione
002800171002     C                   EXSR      sndStato
002900110705     C*
003000921023     C                   SETON                                        LR
003100031103
003200031103
003300031103
003400170912     C     sndStato      BEGSR
003500170912     C*
003600171002     C                   eval      iStatusDate = %editc(iDataEvento:'X')
003700171002     C                   eval      iStatusTime = %editc(iOraEvento:'X')+'00'
003800171002     C*
003900170912     C                   call      'TITV3A6C3'
004000170912     C                   parm                    iAmbiente
004100170912     C                   parm                    iSAPnumDOC
004200170912     C                   parm                    iStatus
004300170912     C                   parm                    iTipoStato
004400170912     C                   parm                    iStatusDate
004500170912     C                   parm                    iStatusTime
004600170912     C                   parm                    oReturnCode
004700170912     C                   parm                    oReturnDescr
004800170912     C*
004900170912     C*  Se esito invocazione servizio non OK
005000170912     C                   if         %trim(oReturnCode) = '0'
005100170912     C                   else
005200170912     C                   eval      wObj =
005300170912     C                             'BARTVAS - Problema invio stati a Telecom'
005400170912     C                   endif
005500031103     C*
005600031103     C                   ENDSR
005700031103
005800031103
005900991027
006000991027      /TITLE Operazioni iniziali.
006100991027     C     *inzsr        BEGSR
006200991027     C*
006300991027     C     *ENTRY        PLIST
006400171002     C                   parm                    iAmbiente
006500171002     C                   parm                    iSAPnumDOC
006600171002     C                   parm                    iStatus
006700171002     C                   parm                    iTipoStato
006800171002     C                   parm                    iDataEvento
006900171002     C                   parm                    iOraEvento
007000991027     C*
007100991027     C                   ENDSR
