000100981204      *----------------------------------------------------*
000200991207      *  Lettura di tutte le CA legate ad una spedizione per attivare il flag su TITAS
000300981204      *----------------------------------------------------*
000400981204
000500981204      *--------------------------------------------------------------*
000600981204      *  RIEPILOGO INDICATORI
000700981204      *--------------------------------------------------------------*
000800981204      * 30 - ERRORE comodo
000900981204      * 31 - ERRORE comodo
001000981204      *--------------------------------------------------------------*
001100981204
001200981204     H DECEDIT('0,') DATEDIT(*DMY.)
001300981204
001400991207     FFNDCT02L  IF   E           K DISK
001500050218     FTITAS30C  UF   E           K DISK    usropn
001600991207      *
001700050222     d cmd             s             60    dim(02) ctdata perrcd(1)
001800050218
001900991207     D fIDN58DS      E DS
002000981204
002100981204     D KPJBA         E DS
002200981204
002300991214     D W_tasfda        S                   like(tasfda)
002400050218     d comman          s             80
002500050218     d lenght          s             15  5 inz(80)
002600050218     d wlib            s             10    inz('GAITRAGRU ')
002700050218     d wlibp           s             10    inz('GAITRAGRPS')
002800981204      *------------------------------------------------------------------------*
002900981204      *
003000981204      *------------------------------------------------------------------------*
003100981204
003200991207      * aggancio bolla
003300991207     c     Kspe          chain     titas30c                           30
003400991207      *
003500991214     c                   IF        not *in30
003600991207     c* pulisco il flag che segnala la presenza della CA
003700991207     c                   clear                   tasfda
003800991207     c* verifico se tutte le CA legate alla spedizione sono annullate
003900991207     c     kspe          setll     fndct02l
004000991207     c*
004100991207     c     kspe          reade     fndct02l                               31
004200991207     c*
004300991207     c                   dow       not *in31
004400991207     c                   if        dctatb = ' '
004500991207     c                   eval      tasfda = 'S'
004600990226     c                   eval      *in31 = *on
004700990226     c                   endif
004800991207     c  n31Kspe          reade     fndct02l                               31
004900991207     c                   enddo
005000990219      *
005100991207     c                   update    titas000
005200991214      * aggiorno anche gli altri records con la stessa spedizione
005300991214     c                   eval      W_TASFDA = tasfda
005400991214     c                   dow       not *in30
005500991214      *
005600991214     c     Kspe          READE     titas30c                               30
005700991214     c   30              LEAVE
005800991214     c                   eval      TASFDA = W_tasfda
005900991214     c                   update    titas000
006000991214      *
006100991214     c                   endDO
006200991214      *
006300991207     c                   endif
006400050218
006500050218     c     fine          Tag
006600050222
006700050222      * cancello le OVRDBF
006800050222     c                   If        i58sed <> 'S'
006900050222     c                   Clear                   comman
007000050222     c                   Movel(p)  cmd(2)        comman
007100050222     c                   Call      'QCMDEXC'                            99
007200050222     c                   Parm                    comman
007300050222     c                   Parm                    lenght
007400050222     c                   EndIf
007500981204      *
007600990217     c                   eval      *inlr = *on
007700981204
007800981204      *------------------------------------------------------------------------*
007900981204      * ROUTINE INIZIALE
008000981204      *------------------------------------------------------------------------*
008100981204     C     *INZSR        BEGSR
008200981204      *
008300981204     C     *ENTRY        PLIST
008400981204     C                   PARM                    KPJBA
008500991207     c                   movel     kpjbu         fidn58ds
008600050218
008700050222     c                   If        i58sed = *Blanks
008800050218      * Apertura del file TITAS
008900050218     c                   Clear                   comman
009000050218     c                   Eval      comman = cmd(1)
009100050218     c                   Eval      %Subst(comman:13:8) = 'TITAS30C'
009200050218      * test sistema informativo di prova
009300050218     c                   If        %subst(knsif:7:1) = 'P'
009400050218     c                   Eval      %Subst(comman:30:10) = wlibp
009500050218     c                   Else
009600050218     c                   Eval      %Subst(comman:30:10) = wlib
009700050218     c                   EndIf
009800050218
009900050218     c                   Eval      comman =
010000050218     c                             %trim(comman) + '/TITAS30C)'
010100050218     c                   Call      'QCMDEXC'                            99
010200050218     c                   Parm                    comman
010300050218     c                   Parm                    lenght
010400050218     c   99              Goto      fine
010500050222     c                   EndIf
010600050218     c  n99              Open      Titas30c
010700981204      *
010800981204     C*----------------------------------------------------*
010900981204     C* KEY   KEY    KEY   KEY   KEY   KEY   KEY   KEY  KEY
011000981204     C*----------------------------------------------------*
011100981204      *
011200991207     c* ACCESSO TITAS/FNDCT
011300981209     C     kspe          KLIST                                                  *
011400991207     C                   KFLD                    i58aas                         *
011500991207     C                   KFLD                    i58lnp                         *
011600991207     C                   KFLD                    i58nrs                         *
011700991207     C                   KFLD                    i58nsp                         *
011800990219     c*
011900981204     C                   ENDSR
012000991207      *-------------------------------------------------------------------------
012100050218** cmd
012200050218OVRDBF FILE(        ) TOFILE(
012300050222DLTOVR FILE(*ALL)
