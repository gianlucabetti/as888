000100120521      /TITLE Upload via Internet: traduzione in FIVABWWR
000200081222     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300120207     H DECEDIT('0,') DATEDIT(*DMY.)
000400990908
000500990910     Ftivin00r  uF   E             DISK    usropn
000600120207     FTIVGD00F  O    E             DISK
000700990908
000800000801     D*----------------------------------------------------
000900000801     D* DICHIARAZIOINE VARIABILI DI WRK
001000000801     D*----------------------------------------------------
001100990910     D psds           sds
001200990910     D  procname         *PROC
001300990920     D tivlrds       e ds                  extname(tivlr00f)
001400120521     D fnvabds       e ds                  extname(fnvab00r)
001500040506     D tisi95ds      e ds
001600990910     D esito           s              1
001700000724     D prmlit          s             10
001800000710     D prmfir          s             10
001900120521     D pptCHR          s              1    inz
002000120521     D pptCSV          s              1    inz
002100990921     D wrkesito        s                   like(esito)
002200120521     D wBufferOut      s           2048    inz
002300990908
002400081222
002500081222     D*------------------
002600081222     D* LINKING A DEFINIZIONI ESTERNE
002700081222     D*------------------
002800081222     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
002900081222     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
003000010201
003100010201
003200990921     C                   reset                   esito
003300990921     C                   reset                   wrkesito
003400000613     C*
003500120521     C                   EXSR      inzvar
003600120521     C                   EXSR      defcam
003700120521     C                   EXSR      wriVGDHEAD
003800040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
003900040929     C*
004000040929     C* Esegue lancio TISI95R solo x chiusura
004100040929     C                   CLEAR                   TISI95DS
004200040929     C                   EVAL      I95TLA = 'C'
004300040929     C                   CALL      'TISI95R'
004400040929     C                   PARM                    TISI95DS
004500000616     C*
004600010201     C                   seton                                        LR
004700000613
004800990908
004900000801
005000910830     C*--------------------------------------------------------
005100120521     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR
005200910830     C*--------------------------------------------------------
005300040526     C     RWFILE        BEGSR
005400990910     C*
005500990914     C                   if        not %open(tivin00r)
005600990908     C                   open      tivin00r
005700990914     C                   endif
005800990910     C*
005900120521     C                   Z-ADD     *zeros        errore            1 0
006000010201     C                   clear                   §CTROKVB          5 0
006100040910     C*
006200921023     C                   DO        *HIVAL
006300990913     C*
006400990915     C                   READ      tivin00r                               70
006500040910     C                   if        vindta > *blanks
006600000801     C*
006700120521     C                   if        *in70 = *off and
006800000801     C                             (vinflg = *blanks
006900000801     C                              or vinflg = '0'
007000000801     C                              or vinflg = '2')
007100000801     C*
007200000801     C                   clear                   vinmsg
007300000801     C                   eval      vinflg = '1'
007400040910     C*
007500040910     C* Eseguo routine d traduzione
007600120521     C                   EXSR      impvab
007700120521     C                   EXSR      wriVGDSHPM
007800040802     C*
007900010305     C                   endif
008000000905     C*
008100000905     C                   else
008200000905     C                   eval      vinflg = '1'
008300000905     C                   endif
008400000905     C*
008500000905     C  N70              update    tivin000
008600000905     C*
008700120521     C  N70              ENDDO
008800120521     C*
008900990910     C* Se non ci sono record con errori ...
009000120521     C                   if        errore = 0
009100990910     C* ... restituisco esito OK.
009200990921     C                   eval      wrkesito = '0'
009300990910     C                   else
009400010201     C                   if        §ctrokvb > 0
009500990921     C                   eval      wrkesito = '1'
009600000710     C                   else
009700000710     C                   eval      wrkesito = '2'
009800990910     C                   endif
009900000710     C                   endif
010000990910     C*
010100990914     C                   if        %open(tivin00r)
010200990908     C                   close     tivin00r
010300990914     C                   endif
010400990920     C*
010500910830     C                   ENDSR
010600000613     C***
010700990920
010800000801     C*----------------------------------------------------*
010900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
011000000801     C*----------------------------------------------------*
011100010201     C     INZVAR        BEGSR
011200000801     C*
011300040802     C                   MOVEL     '0'           FlgCAS            1
011400000801     C*
011500000801     C                   ENDSR
011600000801     C*----------------------------------------------------*
011700040910     C*  IMPOSTAZIONE CAMPI COSTANTI
011800000801     C*----------------------------------------------------*
011900000801     C     DEFCAM        BEGSR
012000000801     C*
012100120521     C                   CLEAR                   fnvabds
012200020619     C* Imposto i valori di default...
012300120521     C                   Z-ADD     1321233       VABCCM
012400120521     C                   Z-ADD     132           VABLNP
012500120521     C                   Z-ADD     000           VABCTR
012600040823     C                   MOVEL     '1'           VABCBO
012700120521     C                   MOVEL     'C'           VABTSP
012800020619     C* ... e poi verifico se sono stati passati come parametri
012900040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
013000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
013100020619     C                   EXSR      CHKNUM
013200020619     C                   IF        PiInt=*on
013300020619     C                   Z-ADD     PiVal         VABCCM
013400020619     C                   ENDIF
013500040506     C                   ENDIF
013600040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
013700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
013800020619     C                   EXSR      CHKNUM
013900020619     C                   IF        PiInt=*on
014000020619     C                   Z-ADD     PiVal         VABLNP
014100040506     C                   ENDIF
014200020619     C                   ENDIF
014300040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
014400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
014500020619     C                   EXSR      CHKNUM
014600020619     C                   IF        PiInt=*on
014700020619     C                   Z-ADD     PiVal         VABCTR
014800040506     C                   ENDIF
014900020619     C                   ENDIF
015000070427     C                   IF        %subst(vlrppt:14:1) <> *blanks
015100070427     C                   EVAL      VABTSP = %subst(vlrppt:14:1)
015200070427     C                   ENDIF
015300120521     C                   IF        %subst(vlrppt:15:1) <> *blanks
015400120521     C                   EVAL      pptCSV = %subst(vlrppt:15:1)
015500120521     C                   ELSE
015600120521     C                   EVAL      pptCSV = ';'
015700120521     C                   ENDIF
015800120521     C                   IF        %subst(vlrppt:16:1) <> *blanks
015900120521     C                   EVAL      pptCHR = %subst(vlrppt:16:1)
016000120521     C                   ELSE
016100120521     C                   EVAL      pptCHR = ' '
016200120521     C                   ENDIF
016300000801     C*
016400000801     C                   ENDSR
016500000801     C*----------------------------------------------------*
016600120521     C*  IMPOSTAZIONE CAMPI DA FLAT FILE
016700000801     C*----------------------------------------------------*
016800120521     C     IMPVAB        BEGSR
016900040910     C*
017000120521     C* Inizializzazioni
017100120521     C                   exsr      inzvar
017200120521     C                   exsr      defcam
017300120521     C*
017400081222     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
017500081222     C                   MOVEL     datcor        VABAAS
017600081222     C                   MOVE      datcor        VABMGS
017700120521     C*
017800120521     C                   EVAL      VABRSD=%trim(%subst(vindta:182:30))
017900060928     C* == sostituisco caratteri che bloccano POSTEL/piccione ==
018000060928     C     '@':'A'       XLATE     VABRSD        VABRSD
018100060928     C     '§':'S'       XLATE     VABRSD        VABRSD
018200060928     C     '!':' '       XLATE     VABRSD        VABRSD
018300060928     C     ':':' '       XLATE     VABRSD        VABRSD
018400120521     C*
018500120521     C                   EVAL      VABIND=%trim(%subst(vindta:212:30))
018600120521     C                   EVAL      VABLOD=%trim(%subst(vindta:242:30))
018700120521     C                   EVAL      VABPRD=%trim(%subst(vindta:272:2))
018800120521     C                   EVAL      VABCAD=%trim(%subst(vindta:274:5))
018900120521     C                   EVAL      VABRMA=%trim(%subst(vindta:9:10))
019000120521     C*
019100040910     C* Reperisco la provincia dal CAP e dalla località
019200040910     C                   IF        VABCAD <> *blanks AND
019300040910     C                             VABPRD  = *blanks
019400040910     C                   CLEAR                   TISI95DS
019500040910     C                   EVAL      I95TCN = '3'
019600040910     C                   Z-ADD     datcor        I95DAT
019700040910     C                   EVAL      I95CAP = VABCAD
019800040910     C                   EVAL      I95LOC = VABLOD
019900040910     C                   CALL      'TISI95R'
020000040910     C                   PARM                    TISI95DS
020100040910     C                   EVAL      VABPRD = O95PRV
020200040910     C                   ENDIF
020300120521     C*
020400120521     C* VABRMN / VABNSP
020500120521     C                   EVAL      PiStr=%trim(%subst(vindta:9:10))
020600120130     C                   EXSR      CHKNUM
020700120130     C                   IF        PiInt=*on
020800120130     C                   ADD       PiVal         VABRMN
020900120521     C                   ADD       PiVal         VABNSP
021000120130     C                   ELSE
021100120130     C                   ADD       1             errore
021200120130     C                   EVAL      vinmsg = %trimr(vinmsg)
021300120521     C                             + ' ' + 'VABRMN VABNSP'
021400120130     C                   ENDIF
021500120521     C* VABNCL
021600120521     C                   EVAL      PiStr=%trim(%subst(vindta:309:5))
021700040910     C                   EXSR      CHKNUM
021800040910     C                   IF        PiInt=*on
021900120521     C                   Z-ADD     PiVal         VABNCL
022000040910     C                   ELSE
022100040910     C                   ADD       1             errore
022200040910     C                   EVAL      vinmsg = %trimr(vinmsg)
022300040910     C                             + ' ' + 'VABNCL'
022400040910     C                   ENDIF
022500120521     C* VABPKB
022600120521     C                   EVAL      PiStr=%trim(%subst(vindta:302:7))
022700040910     C                   EXSR      CHKNUM
022800040910     C                   IF        PiNum=*on
022900120521     C                   EVAL      PiVal=PiVal/100
023000120521     C                   Z-ADD(H)  PiVal         VABPKB                         * gestisco 2 dec
023100040910     C                   ELSE
023200040910     C                   ADD       1             errore
023300040910     C                   EVAL      vinmsg = %trimr(vinmsg)
023400040910     C                             + ' ' + 'VABPKB'
023500040910     C                   ENDIF
023600120521     C* VABCAS
023700120521     C                   IF        %subst(vindta:335:9) <> *zeros AND
023800120521     C                             %subst(vindta:335:9) <> *blanks
023900040910     C                   EVAL      FlgCAS = '1'
024000120521     C                   EVAL      VABVCA='EUR'
024100120521     C                   EVAL      PiStr=%trim(%subst(vindta:335:9))
024200040910     C                   EXSR      CHKNUM
024300040910     C                   IF        PiNum=*on
024400120521     C                   Z-ADD     PiVal         VABCAS
024500040910     C                   ELSE
024600040910     C                   ADD       1             errore
024700040910     C                   EVAL      vinmsg = %trimr(vinmsg)
024800040910     C                             + ' ' + 'VABCAS'
024900040910     C                   ENDIF
025000120521     C                   ENDIF
025100120521     C* VABCBO
025200120521     C                   IF        FlgCAS = '1'
025300120521     C                   EVAL      VABCBO='4'
025400120521     C                   ELSE
025500120521     C                   EVAL      VABCBO='1'
025600120521     C                   ENDIF
025700120521     C* VABNOT
025800120521     C                   EVAL      VABNOT=%trim(%subst(vindta:410:40))
025900010202     C*
026000010201     C                   ADD       1             §CTROKVB
026100000801     C*
026200000801     C                   ENDSR
026300000801     C*----------------------------------------------------*
026400000801     C*  CONTROLLO NUMERICITA' CAMPI
026500000801     C*----------------------------------------------------*
026600000801     C     CHKNUM        BEGSR
026700081222     C*
026800081222     C                   IF        PiDecChr = *blanks
026900081222     C                   EVAL      PiDecChr = ','
027000081222     C                   ENDIF
027100081222     C*
027200081222     C                   callp(e)  UBISNUM_Check(PiStr
027300081222     C                                          :PiDecChr
027400081222     C                                          :PiVal
027500081222     C                                          :PiNum
027600081222     C                                          :PiInt)
027700081222     C*
027800081222     C                   IF        %error
027900081222     C                   EVAL      PiInt=*off
028000081222     C                   ENDIF
028100000801     C*
028200000801     C                   ENDSR
028300000801     C***
028400120207
028500120207
028600120207
028700120521      /TITLE Scrittura record in file TIVGD00F (file VAS generico download)
028800120521     C     wriVGDHEAD    BEGSR
028900120521     C*
029000120521     C                   eval      wBufferOut =
029100120521     C                             %trim(pptCHR)+'VABNSP' +%trim(pptCHR)+pptCSV+
029200120521     C                             %trim(pptCHR)+'VABTSP' +%trim(pptCHR)+pptCSV+
029300120521     C                             %trim(pptCHR)+'VABCTR' +%trim(pptCHR)+pptCSV+
029400120521     C                             %trim(pptCHR)+'VABRMN' +%trim(pptCHR)+pptCSV+
029500120521     C                             %trim(pptCHR)+'VABRMA' +%trim(pptCHR)+pptCSV+
029600120521     C                             %trim(pptCHR)+'VABCBO' +%trim(pptCHR)+pptCSV+
029700120521     C                             %trim(pptCHR)+'VABRSD' +%trim(pptCHR)+pptCSV+
029800120521     C                             %trim(pptCHR)+'VABIND' +%trim(pptCHR)+pptCSV+
029900120521     C                             %trim(pptCHR)+'VABLOD' +%trim(pptCHR)+pptCSV+
030000120521     C                             %trim(pptCHR)+'VABPRD' +%trim(pptCHR)+pptCSV+
030100120521     C                             %trim(pptCHR)+'VABCAD' +%trim(pptCHR)+pptCSV+
030200120521     C                             %trim(pptCHR)+'VABPKB' +%trim(pptCHR)+pptCSV+
030300120521     C                             %trim(pptCHR)+'VABNCL' +%trim(pptCHR)+pptCSV+
030400120521     C                             %trim(pptCHR)+'VABCAS' +%trim(pptCHR)+pptCSV+
030500120521     C                             %trim(pptCHR)+'VABVCA' +%trim(pptCHR)+pptCSV+
030600120521     C                             %trim(pptCHR)+'VABNOT' +%trim(pptCHR)+pptCSV+
030700120613     C                             %trim(pptCHR)+'VABNT2' +%trim(pptCHR)+pptCSV+
030800120613     C                             %trim(pptCHR)+'VABCCM' +%trim(pptCHR)
030900120521     C                   exsr      wriVGD
031000120521     C*
031100120521     C                   ENDSR
031200120521     C*------------------------------------------------------------------------*
031300120521
031400120521
031500120521      /TITLE Scrittura record in file TIVGD00F (file VAS generico download)
031600120521     C     wriVGDSHPM    BEGSR
031700120521     C*
031800120521     C                   eval      wBufferOut =
031900120521     C                             %trim(pptCHR)+%editc(VABNSP:'X')
032000120521     C                                                    +%trim(pptCHR)+pptCSV+
032100120521     C                             %trim(pptCHR)+%trim(VABTSP)
032200120521     C                                                    +%trim(pptCHR)+pptCSV+
032300120521     C                             %trim(pptCHR)+%editc(VABCTR:'X')
032400120521     C                                                    +%trim(pptCHR)+pptCSV+
032500120521     C                             %trim(pptCHR)+%editc(VABRMN:'X')
032600120521     C                                                    +%trim(pptCHR)+pptCSV+
032700120521     C                             %trim(pptCHR)+%trim(VABRMA)
032800120521     C                                                    +%trim(pptCHR)+pptCSV+
032900120521     C                             %trim(pptCHR)+%trim(VABCBO)
033000120521     C                                                    +%trim(pptCHR)+pptCSV+
033100120521     C                             %trim(pptCHR)+%trim(VABRSD)
033200120521     C                                                    +%trim(pptCHR)+pptCSV+
033300120521     C                             %trim(pptCHR)+%trim(VABIND)
033400120521     C                                                    +%trim(pptCHR)+pptCSV+
033500120521     C                             %trim(pptCHR)+%trim(VABLOD)
033600120521     C                                                    +%trim(pptCHR)+pptCSV+
033700120521     C                             %trim(pptCHR)+%trim(VABPRD)
033800120521     C                                                    +%trim(pptCHR)+pptCSV+
033900120521     C                             %trim(pptCHR)+%trim(VABCAD)
034000120521     C                                                    +%trim(pptCHR)+pptCSV+
034100120521     C                             %trim(pptCHR)+%editc(VABPKB:'4')
034200120521     C                                                    +%trim(pptCHR)+pptCSV+
034300120521     C                             %trim(pptCHR)+%editc(VABNCL:'4')
034400120521     C                                                    +%trim(pptCHR)+pptCSV+
034500120521     C                             %trim(pptCHR)+%editc(VABCAS:'4')
034600120521     C                                                    +%trim(pptCHR)+pptCSV+
034700120521     C                             %trim(pptCHR)+%trim(VABVCA)
034800120521     C                                                    +%trim(pptCHR)+pptCSV+
034900120521     C                             %trim(pptCHR)+%trim(VABNOT)
035000120613     C                                                    +%trim(pptCHR)+pptCSV+
035100120613     C                             %trim(pptCHR)+%trim(VABNT2)
035200120613     C                                                    +%trim(pptCHR)+pptCSV+
035300120613     C                             %trim(pptCHR)+%editc(VABCCM:'X')
035400120613     C                                                    +%trim(pptCHR)
035500120521     C                   exsr      wriVGD
035600120521     C*
035700120521     C                   ENDSR
035800120521     C*------------------------------------------------------------------------*
035900120521
036000120521
036100120207      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
036200120207     C     wriVGD        BEGSR
036300120207     C*
036400120207     C                   clear                   tivgd000
036500120521     C                   eval      vgdDTA = wBufferOut
036600120207     C                   eval      vgdTIP = 'EB'
036700120207     C                   eval      vgdKSU = vlrKSC
036800120207     C                   eval      vgdTSC = 'WW'
036900120207     C                   eval      vgdDAT = datcor
037000120521     C                   eval      vgdPGM = 'TITV2L5R'
037100120207     C                   write     tivgd000
037200120207     C*
037300120207     C                   ENDSR
037400120207     C*------------------------------------------------------------------------*
037500120207
037600070411
037700990910
037800000613     C     *inzsr        BEGSR
037900990910     C*
038000990910     C     *entry        plist
038100990920     C                   parm                    tivlrds
038200990921     C                   parm      wrkesito      esito
038300000724     C                   parm                    prmlit
038400000710     C                   parm                    prmfir
038500000613     C*
038600000830     C* CALCOLA LA DATA CORRENTE
038700120207     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
038800120207     C                   eval      datcor = %dec(%date() : *iso)
038900000830     C*
039000000613     C                   ENDSR
039100000613     C***
