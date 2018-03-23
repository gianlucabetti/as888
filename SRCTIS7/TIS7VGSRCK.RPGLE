000100040309      /TITLE Controllo attività UpLoad
000200160312     H DECEDIT('0,') DATEDIT(*DMY.)
000300161206     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000400990907
000500161207     FTIVGS00F  UF   E             DISK    prefix(F0_)
000600161207     FTIVGS10F  UF   E             DISK    prefix(F1_)
000700990907
000800160312     D* Variabili di work
000900161206     D d47JNAM         S             10    inz
001000161206     D d47UNAM         S             10    inz
001100161206     D d47JNUM         S              6    inz
001200161206     D d47JSTS         S             10    inz
001300161206
001400000710
001500120305     C*
001600161206     C* Monitorizzo il processo
001700161206     C                   monitor
001800161206     C*
001900161206     C* Esegue verifica attività di elaborazione transazioni DWL
002000161207     C                   exsr      chkVGS0
002100161207     C                   exsr      chkVGS1
002200161206     C*
002300161206     C* Per errore => nulla
002400161206     C                   on-error
002500161206     C*
002600161206     C* Fine monitoring
002700161206     C                   endmon
002800160312     C*
002900161209     C                   return
003000160312
003100160312
003200160312
003300161206      /TITLE Routine verifica attività di elaborazione transazioni DWL
003400160312     C*
003500161207     C     chkVGS0       begsr
003600161206     C*
003700161209     C     *start        setll     tivgs00f
003800161206     C                   read      tivgs00f
003900161206     C                   dow       not %eof(tivgs00f)
004000161206     C*
004100161206     C* Effettuo chiamata x verificare se il job è ancora attivo
004200161206     C                   call(e)   'TRUL47C'
004300161207     C                   parm      F0_vgsJNAM    d47JNAM
004400161207     C                   parm      F0_vgsUNAM    d47UNAM
004500161207     C                   parm      F0_vgsJNUM    d47JNUM
004600161206     C                   parm      *blanks       d47JSTS
004700161206     C*
004800161206     C* Se reprito  stati job e job terminato => elimino blocco da VGS
004900161206     C                   if        not %error
005000161206     C                   if        d47JSTS = '*OUTQ'  OR
005100161206     C                             d47JSTS = '*ERROR'
005200161206     C                   delete    tivgs00f
005300161206     C                   endif
005400161206     C                   endif
005500161206     C*
005600161206     C                   read      tivgs00f
005700161206     C                   enddo
005800160312     C*
005900160312     C                   endsr
006000161207
006100161207
006200161207
006300161207      /TITLE Routine verifica attività di elaborazione transazioni DWL
006400161207     C*
006500161207     C     chkVGS1       begsr
006600161207     C*
006700161209     C     *start        setll     tivgs10f
006800161207     C                   read      tivgs10f
006900161207     C                   dow       not %eof(tivgs10f)
007000161207     C*
007100161207     C* Effettuo chiamata x verificare se il job è ancora attivo
007200161207     C                   call(e)   'TRUL47C'
007300161207     C                   parm      F1_vgsJNAM    d47JNAM
007400161207     C                   parm      F1_vgsUNAM    d47UNAM
007500161207     C                   parm      F1_vgsJNUM    d47JNUM
007600161207     C                   parm      *blanks       d47JSTS
007700161207     C*
007800161207     C* Se reprito  stati job e job terminato => elimino blocco da VGS
007900161207     C                   if        not %error
008000161207     C                   if        d47JSTS = '*OUTQ'  OR
008100161207     C                             d47JSTS = '*ERROR'
008200161207     C                   delete    tivgs10f
008300161207     C                   endif
008400161207     C                   endif
008500161207     C*
008600161207     C                   read      tivgs10f
008700161207     C                   enddo
008800161207     C*
008900161207     C                   endsr
009000000714
009100000714
009200000714
009300000714      /TITLE Routine di *init PGM
009400000714     C*
009500000714     C     *inzsr        begsr
009600000714     C*
009700000714     C                   endsr
009800000714
