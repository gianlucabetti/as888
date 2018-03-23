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
003100061211     c                   clear                   o58fda
003200981204
003300991207      * aggancio bolla
003400991207     c     Kspe          chain     titas30c                           30
003500991207      *
003600061211     c* da sede cerco c.a. anche se non c'e' la bolla (richiamo
003700061211     c*  da TRTR36R)
003800061211    1c                   IF        not *in30  or
003900061211     c                             (*in30 and i58sed='S')
004000061211     c
004100991207     c* pulisco il flag che segnala la presenza della CA
004200991207     c                   clear                   tasfda
004300991207     c* verifico se tutte le CA legate alla spedizione sono annullate
004400991207     c     kspe          setll     fndct02l
004500991207     c*
004600991207     c     kspe          reade     fndct02l                               31
004700991207     c*
004800061211    2c                   dow       not *in31
004900991207     c                   if        dctatb = ' '
005000991207     c                   eval      tasfda = 'S'
005100990226     c                   eval      *in31 = *on
005200990226     c                   endif
005300991207     c  n31Kspe          reade     fndct02l                               31
005400061211    2c                   enddo
005500061211     c
005600061211      * dato di output a prescindere dalla esistenza della bolla su titas
005700061211     c                   eval      o58fda   = tasfda
005800061211     c*
005900061211    2c                   if        not *in30
006000991207     c                   update    titas000
006100061211     c
006200991214      * aggiorno anche gli altri records con la stessa spedizione
006300991214     c                   eval      W_TASFDA = tasfda
006400061211    3c                   dow       not *in30
006500991214      *
006600991214     c     Kspe          READE     titas30c                               30
006700991214     c   30              LEAVE
006800991214     c                   eval      TASFDA = W_tasfda
006900991214     c                   update    titas000
007000991214      *
007100061211    3c                   endDO
007200991214      *
007300061211    2c                   endif
007400061211    1c                   endif
007500061211     c
007600061211     c                   movel     fidn58ds      kpjbu
007700050218
007800050218     c     fine          Tag
007900050222
008000050222      * cancello le OVRDBF
008100050222     c                   If        i58sed <> 'S'
008200050222     c                   Clear                   comman
008300050222     c                   Movel(p)  cmd(2)        comman
008400050222     c                   Call      'QCMDEXC'                            99
008500050222     c                   Parm                    comman
008600050222     c                   Parm                    lenght
008700050222     c                   EndIf
008800981204      *
008900990217     c                   eval      *inlr = *on
009000981204
009100981204      *------------------------------------------------------------------------*
009200981204      * ROUTINE INIZIALE
009300981204      *------------------------------------------------------------------------*
009400981204     C     *INZSR        BEGSR
009500981204      *
009600981204     C     *ENTRY        PLIST
009700981204     C                   PARM                    KPJBA
009800991207     c                   movel     kpjbu         fidn58ds
009900050218
010000050222     c                   If        i58sed = *Blanks
010100050218      * Apertura del file TITAS
010200050218     c                   Clear                   comman
010300050218     c                   Eval      comman = cmd(1)
010400050218     c                   Eval      %Subst(comman:13:8) = 'TITAS30C'
010500050218      * test sistema informativo di prova
010600050218     c                   If        %subst(knsif:7:1) = 'P'
010700050218     c                   Eval      %Subst(comman:30:10) = wlibp
010800050218     c                   Else
010900050218     c                   Eval      %Subst(comman:30:10) = wlib
011000050218     c                   EndIf
011100050218
011200050218     c                   Eval      comman =
011300050218     c                             %trim(comman) + '/TITAS30C)'
011400050218     c                   Call      'QCMDEXC'                            99
011500050218     c                   Parm                    comman
011600050218     c                   Parm                    lenght
011700050218     c   99              Goto      fine
011800050222     c                   EndIf
011900050218     c  n99              Open      Titas30c
012000981204      *
012100981204     C*----------------------------------------------------*
012200981204     C* KEY   KEY    KEY   KEY   KEY   KEY   KEY   KEY  KEY
012300981204     C*----------------------------------------------------*
012400981204      *
012500991207     c* ACCESSO TITAS/FNDCT
012600981209     C     kspe          KLIST                                                  *
012700991207     C                   KFLD                    i58aas                         *
012800991207     C                   KFLD                    i58lnp                         *
012900991207     C                   KFLD                    i58nrs                         *
013000991207     C                   KFLD                    i58nsp                         *
013100990219     c*
013200981204     C                   ENDSR
013300991207      *-------------------------------------------------------------------------
013400050218** cmd
013500050218OVRDBF FILE(        ) TOFILE(
013600050222DLTOVR FILE(*ALL)
