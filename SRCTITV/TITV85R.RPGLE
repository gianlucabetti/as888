000100020916      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     D*------------
000500030811     D* DS ESTERNE
000600030811     D*------------
000700040109     D tivlrds       e ds                  extname(tivlr00f)
000800990910     D psds           sds
000900990910     D  procname         *PROC
001000040109     D*------------
001100040109     D* VARIABILI DI WRK
001200040109     D*------------
001300990910     D esito           s              1
001400000724     D prmlit          s             10
001500000710     D prmfir          s             10
001600040109     D wMBT            s             10    inz
001700040109     D wPGT            s             10    inz
001800040113     D wPPT            s             50    inz
001900010606
002000010606
002100010606
002200010606
002300990921     C                   reset                   esito
002400010601     C*
002500040109     C                   exsr      opeini
002600040109     C                   exsr      runelab
002700010601     C*
002800010601     C                   seton                                        lr
002900040109
003000040109
003100010601
003200010601
003300040109     C     runelab       BEGSR
003400010601     C*
003500040109     C* Effettuo chiamata a cl d ingresso x preparazione file/membro d putput ed elaborazione
003600040109     C                   call(e)   'TITV85C'
003700040109     C                   parm                    vlrfou
003800040109     C                   parm                    wMBT
003900040109     C                   parm                    vlrksc
004000040109     C                   parm                    wPGT
004100040109     C                   parm                    wPPT
004200040109     C                   parm                    tivlrds
004300040109     C                   parm                    esito
004400040109     C                   parm                    prmlit
004500040109     C                   parm                    prmfir
004600040109     C*
004700040109     C* Gestisco l'eventuale errore in chiamata traduzione
004800040109     C                   if        %error
004900040109     C                   eval      esito = '2'
005000040109     C                   endif
005100040109     C*
005200010601     C                   ENDSR
005300010601     C***
005400040109
005500040109
005600040109
005700040109
005800040109     C     opeini        BEGSR
005900040109     C*
006000040109     C* Verifico se impostato il parametro relativo al nome pgm traduttore
006100040109     C                   if        %subst(vlrppt:1:10) <> *blanks
006200040109     C                   eval      wPGT = %subst(vlrppt:1:10)
006300040109     C                   else
006400040109     C                   eval      wPGT = 'TITV86R'                             * default
006500040109     C                   endif
006600040109     C*
006700040109     C* Verifico se impostato il nome membro del file di output
006800040109     C                   if        %subst(vlrppt:11:10) <> *blanks
006900040109     C                   eval      wMBT = %subst(vlrppt:11:10)
007000040109     C                   else
007100040109     C                   eval      wMBT = vlrprg                                * default
007200040109     C                   endif
007300040109     C*
007400040109     C* Verifico se impostato altri parametri da passare al traduttore precedentemete indicato
007500040109     C                   if        %subst(vlrppt:21:30) <> *blanks
007600040109     C                   eval      wPPT = %subst(vlrppt:21:30)
007700040109     C                   endif
007800040109     C*
007900040109     C                   ENDSR
008000040109     C***
008100010601
008200010601
008300990910
008400000613     C     *inzsr        BEGSR
008500990910     C*
008600990910     C     *entry        plist
008700990920     C                   parm                    tivlrds
008800040109     C                   parm                    esito
008900000724     C                   parm                    prmlit
009000000710     C                   parm                    prmfir
009100040109     C*
009200000613     C                   ENDSR
009300000613     C***
