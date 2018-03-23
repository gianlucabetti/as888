000100040203      /TITLE Upload via FTP: traduzione file LED WWRCE00F
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500040203     FWWRCE00F  O    E             DISK    usropn
000600030613     Ftis7pxp   O    f  132        PRINTER usropn
000700000725     F                                     oflind(*inoa)
000800030613     Ftis7pxps  O    f  198        PRINTER usropn
000900000621     F                                     oflind(*inob)
001000990908
001100000512     D*------------
001200000512     D* COMANDI
001300000512     D*------------
001400090121     D cmd             S             80    DIM(9) CTDATA PERRCD(1)
001500000512     D*------------
001600990910     D psds           sds
001700990910     D  procname         *PROC
001800990920     D tivlrds       e ds                  extname(tivlr00f)
001900040203     D passaggio     e ds                  extname(wwrce00f)
002000990910     D esito           s              1
002100000724     D prmlit          s             10
002200000710     D prmfir          s             10
002300990921     D wrkesito        s                   like(esito)
002400990915     D wrkdata         s               d
002500990915     D wrkora          s               t
002600000613     D rrnum           s              6  0 INZ(*zeros)
002700000621     D recko           s            150    INZ(*blanks)
002800000724     D depcmd          s            100    INZ(*blanks)
002900000724     D §ctrok          s              5  0 INZ(*zeros)
003000000724     D §ctrno          s              5  0 INZ(*zeros)
003100090109     D wNomeFile       s             30    INZ(*blanks)
003200090121     D CurrOpz         s              2    INZ
003300090121     D CurrFileAS      s             10    INZ
003400090129     D CurrPostPgm     s             10    INZ
003500090129     D CurrPostLib     s             10    INZ
003600090121     D CurrSito        s              2    INZ
003700090121     D CurrDepot       s              8    INZ
003800090121     D CurrEnv         s              1    INZ
003900090205     D CurrTip         s              3    INZ
004000090109     D*-------------------
004100090109     D* COSTANTI
004200090109     D*-------------------
004300090109     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
004400090109     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
004500990908
004600990915     C                   time                    wrkdata
004700990915     C                   time                    wrkora
004800000913     C                   reset                   rrnum
004900990921     C                   reset                   esito
005000990921     C                   reset                   wrkesito
005100090109     C*
005200090109     C* EFFETTUO CONSIDERAZIONI SUL FILE DESTINAZINOE
005300090121     C                   EVAL      CurrOpz     = *blanks
005400090121     C                   EVAL      CurrFileAS  = *blanks
005500090129     C                   EVAL      CurrPostPgm = *blanks
005600090121     C                   EVAL      CurrEnv     = %trim(%subst(vlrppt:1:1))
005700090121     C                   EVAL      CurrSito    = %trim(%subst(vlrppt:22:2))
005800090121     C                   EVAL      CurrDepot   = %trim(%subst(vlrppt:24:8))
005900090129     C                   EVAL      CurrPostLib = %trim(%subst(vlrppt:32:10))
006000090121     C*
006100090121     C                   IF        %trim(%subst(vlrppt:2:10)) = '*FILE'
006200090109     C                   MOVE(P)   vlrMSG        wNomeFile
006300090109     C* Porto il nome file sempre in maiuscolo
006400090109     C     minu:maiu     XLATE     wNomeFile     wNomeFile
006500090109     C*
006600090109     C* ...verifico particolarità cliente...
006700090109     C*
006800111227     C* => LIMONI
006900090206     C                   IF        CurrDepot = 'LIMONI'
007000090205     C*
007100111227     C                   IF        %subst(wNomeFile:1:3) = 'ART'
007200090112     C                   EVAL      CurrOpz     = 'GO'
007300090205     C                   EVAL      CurrFileAS  = 'WP3LARP'
007400120103     C                   EVAL      CurrPostPgm = 'LTCB113C'
007500090205     C                   EVAL      CurrTip     = 'ART'
007600090109     C                   ENDIF
007700090205     C*
007800111227     C                   IF        %subst(wNomeFile:1:3) = 'EAN'
007900090112     C                   EVAL      CurrOpz     = 'GO'
008000090205     C                   EVAL      CurrFileAS  = 'WP3LEAP'
008100120103     C                   EVAL      CurrPostPgm = 'LTCB113C'
008200090205     C                   EVAL      CurrTip     = 'EAN'
008300090109     C                   ENDIF
008400090205     C*
008500111227     C                   IF        %subst(wNomeFile:1:3) = 'AFO'
008600090112     C                   EVAL      CurrOpz     = 'GO'
008700090205     C                   EVAL      CurrFileAS  = 'WP3LAFP'
008800120103     C                   EVAL      CurrPostPgm = 'LTCB113C'
008900090205     C                   EVAL      CurrTip     = 'AFO'
009000090112     C                   ENDIF
009100090205     C*
009200111227     C                   IF        %subst(wNomeFile:1:3) = 'DES'
009300090112     C                   EVAL      CurrOpz     = 'GO'
009400090205     C                   EVAL      CurrFileAS  = 'WP3LDEP'
009500120103     C                   EVAL      CurrPostPgm = 'LTCB113C'
009600090205     C                   EVAL      CurrTip     = 'DES'
009700090109     C                   ENDIF
009800090205     C*
009900111227     C                   IF        %subst(wNomeFile:1:3) = 'NOM'
010000090205     C                   EVAL      CurrOpz     = 'GO'
010100090205     C                   EVAL      CurrFileAS  = 'WP3LNOP'
010200120103     C                   EVAL      CurrPostPgm = 'LTCB113C'
010300090205     C                   EVAL      CurrTip     = 'NOM'
010400090205     C                   ENDIF
010500090205     C*
010600111227     C                   IF        %subst(wNomeFile:1:3) = 'NLS'
010700090205     C                   EVAL      CurrOpz     = 'GO'
010800090205     C                   EVAL      CurrFileAS  = 'WP3LNLP'
010900120103     C                   EVAL      CurrPostPgm = 'LTCB113C'
011000090205     C                   EVAL      CurrTip     = 'NLS'
011100090205     C                   ENDIF
011200090205     C*
011300111227     C                   IF        %subst(wNomeFile:1:3) = 'DBA'
011400090205     C                   EVAL      CurrOpz     = 'GO'
011500090205     C                   EVAL      CurrFileAS  = 'WP3LDBP'
011600120103     C                   EVAL      CurrPostPgm = 'LTCB113C'
011700090205     C                   EVAL      CurrTip     = 'DBA'
011800090205     C                   ENDIF
011900090205     C*
012000111227     C                   IF        %subst(wNomeFile:1:3) = 'MAR'
012100090205     C                   EVAL      CurrOpz     = 'GO'
012200090205     C                   EVAL      CurrFileAS  = 'WP3LMAP'
012300120103     C                   EVAL      CurrPostPgm = 'LTCB113C'
012400090205     C                   EVAL      CurrTip     = 'MAR'
012500090205     C                   ENDIF
012600090205     C*
012700111227     C                   IF        %subst(wNomeFile:1:3) = 'LIN'
012800090205     C                   EVAL      CurrOpz     = 'GO'
012900090205     C                   EVAL      CurrFileAS  = 'WP3LLIP'
013000120103     C                   EVAL      CurrPostPgm = 'LTCB113C'
013100090205     C                   EVAL      CurrTip     = 'LIN'
013200090205     C                   ENDIF
013300090205     C*
013400111227     C                   IF        %subst(wNomeFile:1:3) = 'ALS'
013500090205     C                   EVAL      CurrOpz     = 'GO'
013600090205     C                   EVAL      CurrFileAS  = 'WP3LALP'
013700120103     C                   EVAL      CurrPostPgm = 'LTCB113C'
013800090205     C                   EVAL      CurrTip     = 'ALS'
013900090205     C                   ENDIF
014000090205     C*
014100111227     C                   IF        %subst(wNomeFile:1:3) = 'SET'
014200090205     C                   EVAL      CurrOpz     = 'GO'
014300090205     C                   EVAL      CurrFileAS  = 'WP3LSEP'
014400120103     C                   EVAL      CurrPostPgm = 'LTCB113C'
014500090205     C                   EVAL      CurrTip     = 'SET'
014600090205     C                   ENDIF
014700090206     C*
014800111227     C                   IF        %subst(wNomeFile:1:3) = 'BEM'
014900090206     C                   EVAL      CurrOpz     = 'GO'
015000090206     C                   EVAL      CurrFileAS  = 'WP3BEMP'
015100120103     C                   EVAL      CurrPostPgm = 'LTCB113C'
015200090206     C                   EVAL      CurrTip     = 'BEM'
015300090206     C                   ENDIF
015400090205     C*
015500090109     C                   ENDIF
015600090109     C*
015700090109     C                   ELSE
015800090112     C                   EVAL      CurrOpz     = 'GO'
015900090121     C                   EVAL      CurrFileAS  = %trim(%subst(vlrppt:2:10))
016000090129     C                   EVAL      CurrPostPgm = %trim(%subst(vlrppt:12:10))
016100090109     C                   ENDIF
016200090109     C*
016300090109     C* VERIFICO LE CONDIZIONI ATTUALI (SE TUTTO OK)
016400090109     C                   select
016500090109     C                   when      CurrOpz = *blanks
016600090109     C                   eval      wrkesito = '2'
016700090109     C                   when      CurrOpz = 'GO'
016800090109     C*
016900090109     C* OPERAZIONI INIZIALI
017000090109     C                   exsr      PREELA
017100000613     C*
017200090109     C* ELABORAZIONE
017300000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
017400000725     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
017500090109     C*
017600090112     C* RICHIEMO *PGM IN USCITA (se richiesto)
017700090129     C                   if        CurrPostPgm <> *blanks AND
017800090129     C                             CurrPostPgm <> '*NONE'
017900090129     C                   if        CurrPostLib = *blanks
018000090129     C                   if        CurrEnv = 'T'
018100150609     C                   eval      CurrPostLib = 'GEOV72SBT'
018200090129     C                   else
018300150609     C                   eval      CurrPostLib = 'GEOV72SB'
018400090129     C                   endif
018500090129     C                   endif
018600090121     C*
018700120103     C                   call      'TIS7P1C4'
018800090129     C                   parm                    CurrPostLib
018900090129     C                   parm                    CurrPostPgm
019000090121     C                   parm                    CurrEnv
019100090121     C                   parm                    CurrSito
019200090121     C                   parm                    CurrDepot
019300090121     C                   parm                    CurrTip
019400090121     C                   parm                    wrkesito
019500090112     C                   endif
019600090121     C*
019700090121     C* OPERAZIONI FINALI
019800090121     C                   exsr      ENDELA
019900090112     C*
020000090109     C                   endsl
020100000616     C*
020200090109     C                   seton                                        lr
020300090109
020400090109
020500090109     C*--------------------------------------------------------
020600090109     C* PREELA - ROUTINE OPERAZIONI INIZIALI                  *
020700090109     C*--------------------------------------------------------
020800090109     C     PREELA        BEGSR
020900090109     C*
021000090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
021100090109     C                   movel(p)  CMD(1)        depcmd
021200090109     C                   EVAL      depcmd=%trim(depcmd)+'GEODETR'+')'
021300090109     C*
021400090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA X LED
021500090109     C                   Z-ADD     100           LENGH            15 5
021600090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
021700090109     C                   PARM                    depcmd
021800090109     C                   PARM                    LENGH
021900090109     C*
022000090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
022100090109     C                   movel(p)  CMD(2)        depcmd
022200090109     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
022300090109     C*
022400090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
022500090109     C                   Z-ADD     100           LENGH            15 5
022600090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
022700090109     C                   PARM                    depcmd
022800090109     C                   PARM                    LENGH
022900090109     C*
023000090109     C                   if        not %open(tis7pxps)
023100090109     C                   open      tis7pxps
023200090109     C                   except    testdett
023300090109     C                   endif
023400090109     C*
023500090109     C                   ENDSR
023600090109     C***
023700090109
023800090109
023900090109     C*--------------------------------------------------------
024000090109     C* ENDELA - ROUTINE OPERAZIONI FINALI                    *
024100090109     C*--------------------------------------------------------
024200090109     C     ENDELA        BEGSR
024300090109     C*
024400090109     C                   if        %open(tis7pxps)
024500090109     C                   except    findett
024600090109     C                   close     tis7pxps
024700090109     C                   endif
024800090109     C*
024900090109     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
025000090109     C                   movel(p)  CMD(3)        depcmd
025100090109     C                   Z-ADD     100           LENGH            15 5
025200090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
025300090109     C                   PARM                    depcmd
025400090109     C                   PARM                    LENGH
025500090109     C*
025600090109     C                   ENDSR
025700090109     C***
025800090109
025900090109
026000000613
026100000724
026200910830     C*--------------------------------------------------------
026300040203     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WWRCE00F     *
026400910830     C*--------------------------------------------------------
026500000724     C     RWFIL         BEGSR
026600990910     C*
026700990914     C                   if        not %open(tivin00r)
026800990908     C                   open      tivin00r
026900990914     C                   endif
027000040203     C                   if        not %open(wwrce00f)
027100040203     C                   open      wwrce00f
027200990914     C                   endif
027300990910     C*
027400990910     C                   clear                   §CTROK
027500990910     C                   clear                   §CTRNO
027600001009     C                   clear                   recko
027700990910     C*
027800921023     C                   DO        *HIVAL
027900990913     C*
028000990915     C                   READ      tivin00r                               70
028100000905     C                   if        vindta > *blanks
028200000613     C                   add       1             rrnum
028300000724     C                   if        *in70 = *off and
028400990913     C                             (vinflg = *blanks
028500990913     C                              or vinflg = '0'
028600990913     C                              or vinflg = '2')
028700990913     C*
028800990913     C                   clear                   vinmsg
028900000724     C                   if        vindta <> *blanks
029000000724     C                   movel(p)  vindta        passaggio
029100040203     C                   write(e)  WWRCE
029200990910     C*
029300000724     C                   if        not %error
029400000724     C                   add       1             §ctrok
029500990910     C                   eval      vinflg = '1'
029600990910     C                   else
029700000724     C                   add       1             §ctrno
029800000621     C                   eval      recko = vindta
029900000616     C                   except    rigadett
030000990910     C                   eval      vinflg = '2'
030100990910     C                   endif
030200000724     C                   endif
030300991022     C                   endif
030400000905     C*
030500000905     C                   else
030600000905     C                   eval      vinflg = '1'
030700000905     C                   endif
030800000905     C*
030900000905     C  N70              update    tivin000
031000991022     C*
031100991022     C  N70              ENDdo
031200990910
031300990910     C* Se non ci sono record con errori ...
031400000710     C                   if        §ctrno = 0
031500990910     C* ... restituisco esito OK.
031600990921     C                   eval      wrkesito = '0'
031700990910     C                   else
031800000710     C                   if        §ctrok > 0
031900990921     C                   eval      wrkesito = '1'
032000000710     C                   else
032100000710     C                   eval      wrkesito = '2'
032200990910     C                   endif
032300000710     C                   endif
032400990910     C*
032500990914     C                   if        %open(tivin00r)
032600990908     C                   close     tivin00r
032700990914     C                   endif
032800040203     C                   if        %open(wwrce00f)
032900040203     C                   close     wwrce00f
033000990914     C                   endif
033100990920     C*
033200000724     C                   if        wrkesito <> '2'
033300000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
033400070913     C                   movel(p)  CMD(8)        depcmd
033500000724     C                   eval      depcmd = %trim(depcmd) + ' '+
033600000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
033700000724     C                   Z-ADD     100           LENGH            15 5
033800000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
033900000724     C                   PARM                    depcmd
034000000724     C                   PARM                    LENGH
034100000724     C*
034200070913     C                   movel(p)  CMD(9)        depcmd
034300000724     C                   eval      depcmd = %trim(depcmd)
034400000724     C                   Z-ADD     100           LENGH            15 5
034500000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
034600000724     C                   PARM                    depcmd
034700000724     C                   PARM                    LENGH
034800000724     C*
034900000724     C                   movel(p)  CMD(4)        depcmd
035000000724     C                   eval      depcmd = %trim(depcmd) +
035100090109     C                             %trim(prmlit) + %trim(CMD(5)) +
035200090109     C                             %trim(CurrFileAS) +
035300070913     C                             %trim(CMD(6)) +
035400000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
035500070913     C                             %trim(CMD(7))
035600000724     C                   Z-ADD     100           LENGH            15 5
035700000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
035800000724     C                   PARM                    depcmd
035900000724     C                   PARM                    LENGH
036000000724     C*
036100000724     C                   endif
036200000724     C*
036300910830     C                   ENDSR
036400000613     C***
036500000725
036600000725
036700000725     C*--------------------------------------------------------
036800000725     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
036900000725     C*--------------------------------------------------------
037000000725     C     STPR          BEGSR
037100000725     C*
037200030613     C                   if        not %open(tis7pxp)
037300030613     C                   open      tis7pxp
037400000725     C                   endif
037500000725     C*
037600000725     C                   except    riepilogo
037700000725     C*
037800030613     C                   if        %open(tis7pxp)
037900030613     C                   close     tis7pxp
038000000725     C                   endif
038100000725     C*
038200000725     C                   ENDSR
038300000725     C***
038400990920
038500990910
038600000613     C     *inzsr        BEGSR
038700990910     C*
038800990910     C     *entry        plist
038900990920     C                   parm                    tivlrds
039000990921     C                   parm      wrkesito      esito
039100000724     C                   parm                    prmlit
039200000724     C                   parm                    prmfir
039300000613     C*
039400000613     C                   ENDSR
039500000613     C***
039600000725
039700030613     Otis7pxp   E            riepilogo         2
039800000725     O                                              'Upload via FTP'
039900000725     O                                           +1 'Traduzione TIVIN00R -'
040000040203     O                                           +1 'WWRCE00F'
040100000725     O                                           +1 'Entrate'
040200000725     O          E            riepilogo   2
040300000725     O                       wrkdata
040400000725     O                       wrkora              +1
040500000725     O                       procname            +1
040600000725     O          E            riepilogo   2
040700000725     O                                              'Cliente..................:'
040800030613     O                       VLRKSC              +1
040900000725     O          E            riepilogo   2
041000000725     O                                              'Riferimento Strategi.....:'
041100000725     O                       vlrhdl              +1
041200000725     O          E            riepilogo   2
041300000725     O                                              'Giusti...................:'
041400000725     O                       §CTROK        2   +  1
041500000725     O          E            riepilogo   2
041600000725     O                                              'Sbagliati e scartati.....:'
041700000725     O                       §CTRNO        2   +  1
041800990908
041900000724
042000030613     Otis7pxps  E            testdett          2
042100000724     O                                              'Upload via FTP'
042200000613     O                                           +1 'Traduzione TIVIN00R -'
042300040203     O                                           +1 'WWRCE00F'
042400000725     O                                           +1 'Entrate'
042500000616     O          E            testdett    3
042600000613     O                                           +2 'N° rec'
042700000613     O                                           +3 'Anteprima contenuto'
042800000616     O          E            rigadett    2
042900000613     O                       rrnum               +2
043000000621     O                       recko               +3
043100000616     O          E            findett     2
043200000613     O                       wrkdata
043300000613     O                       wrkora              +1
043400000613     O                       procname            +1
043500000616     O          E            findett     2
043600000613     O                                              'Cliente..................:'
043700030613     O                       VLRKSC              +1
043800000616     O          E            findett     2
043900000613     O                                              'Riferimento Strategi.....:'
044000000613     O                       vlrhdl              +1
044100000616     O          E            findett     2
044200000613     O                                              'Giusti...................:'
044300000613     O                       §CTROK        2   +  1
044400000616     O          E            findett     2
044500000613     O                                              'Sbagliati e scartati.....:'
044600000613     O                       §CTRNO        2   +  1
044700000512** CMD - COMANDI CL
044800030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXP)  OUTQ(
044900030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXPS) OUTQ(
045000030613DLTOVR FILE(TIS7PXP TIS7PXPS) LVL(*)
045100040203CPYF FROMFILE(WWRCE00F) TOFILE(
045200070913/
045300070913) FROMMBR(
045400070913) MBROPT(*ADD) FMTOPT(*NOCHK)
045500040203DLCOBJ OBJ((WWRCE00F *FILE *EXCL
045600040203DLTOVR FILE(WWRCE00F) LVL(*JOB)
