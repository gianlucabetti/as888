000100080925      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200090402     H DFTACTGRP(*NO) BNDDIR('QC2LE') ACTGRP('BARTVAS')
000300990908
000400090402     F
000500090402     F
000600090402     F
000700990908
000800000801     D*----------------------------------------------------
000900000801     D* DICHIARAZIOINE VARIABILI DI WRK
001000000801     D*----------------------------------------------------
001100990910     D psds           sds
001200990910     D  procname         *PROC
001300090402     D tivlrds       e ds                  extname(tivlr00f)
001400990910     D esito           s              1
001500000724     D prmlit          s             10
001600000710     D prmfir          s             10
001700990921     D wrkesito        s                   like(esito)
001800081104
001900081104
002000081104     D*------------------
002100081104     D* PASSAGGIO PARAMETRI A PROCEDURE SYSTEM
002200081104     D*------------------
002300090402     D executeCommand...
002400090402     D                 PR            10I 0 EXTPROC('system')
002500090402     D  command                        *   VALUE OPTIONS(*STRING)
002600090402     D rc              S             10I 0
002700090402     D exceptionMessageID...
002800090402     D                 S              7A   IMPORT('_EXCP_MSGID')
002900090402
003000080925
003100081029
003200081104      /free
003300090402
003400090402
003500090402        // Effettuo la copia sul file indicato nei parametri in anagrafica
003600090402        if %subst(vlrppt:1:21)<>*blanks;
003700090402           exsr copiaFile;
003800090402        else;
003900090402           eval wrkesito = '2';
004000090402        endif;
004100090402
004200090402
004300090402        *inlr = *on;
004400081104
004500090402
004600090402
004700090402
004800090402        Begsr copiaFile;
004900090402
005000090402
005100120725              exceptionMessageID = *blanks;
005200090402              if executeCommand('CPYF FROMFILE(TIVIN00R) TOFILE(' +
005300090402                                %trim(%subst(vlrppt:1:21))        +
005400090402                                ') MBROPT(*ADD) FMTOPT(*NOCHK)') <> *zeros;
005500090402                 wrkesito = '2';
005600090402              else;
005700090402                 if exceptionMessageID <> *blanks;
005800090402                    wrkesito = '2';
005900090402                 else;
006000090402                    wrkesito = '0';
006100090402                 endif;
006200090402              endif;
006300090402
006400090402
006500090402        Endsr;
006600090402
006700090402
006800090402      /end-free
006900090402
007000090402
007100090402
007200070411     C     *pssr         BEGSR
007300090402     C*
007400070411     C                   eval      wrkesito = '2'
007500070411     C*
007600070411     C                   seton                                        LR
007700070411     C*
007800070411     C                   ENDSR     '*CANCL'
007900070411     C***
008000090402
008100090402
008200000613     C     *inzsr        BEGSR
008300990910     C*
008400990910     C     *entry        plist
008500990920     C                   parm                    tivlrds
008600990921     C                   parm      wrkesito      esito
008700000724     C                   parm                    prmlit
008800000710     C                   parm                    prmfir
008900000830     C*
009000000613     C                   ENDSR
009100000613     C***
