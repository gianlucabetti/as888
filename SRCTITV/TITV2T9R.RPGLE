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
001500150507     D fnvatds       e ds                  extname(fnvat00f)
001600150507     D VATNOT_B        s                   like(VATNOT) inz
001700040506     D tisi95ds      e ds
001800990910     D esito           s              1
001900000724     D prmlit          s             10
002000000710     D prmfir          s             10
002100120521     D pptCHR          s              1    inz
002200120521     D pptCSV          s              1    inz
002300990921     D wrkesito        s                   like(esito)
002400120521     D wBufferOut      s           2048    inz
002500990908
002600081222
002700081222     D*------------------
002800081222     D* LINKING A DEFINIZIONI ESTERNE
002900081222     D*------------------
003000081222     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
003100081222     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
003200010201
003300010201
003400990921     C                   reset                   esito
003500990921     C                   reset                   wrkesito
003600000613     C*
003700120521     C                   EXSR      inzvar
003800120521     C                   EXSR      defcam
003900120521     C                   EXSR      wriVGDHEAD
004000040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004100040929     C*
004200040929     C* Esegue lancio TISI95R solo x chiusura
004300040929     C                   CLEAR                   TISI95DS
004400040929     C                   EVAL      I95TLA = 'C'
004500040929     C                   CALL      'TISI95R'
004600040929     C                   PARM                    TISI95DS
004700000616     C*
004800010201     C                   seton                                        LR
004900000613
005000990908
005100000801
005200910830     C*--------------------------------------------------------
005300120521     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR
005400910830     C*--------------------------------------------------------
005500040526     C     RWFILE        BEGSR
005600990910     C*
005700990914     C                   if        not %open(tivin00r)
005800990908     C                   open      tivin00r
005900990914     C                   endif
006000990910     C*
006100120521     C                   Z-ADD     *zeros        errore            1 0
006200010201     C                   clear                   §CTROKVB          5 0
006300040910     C*
006400921023     C                   DO        *HIVAL
006500990913     C*
006600990915     C                   READ      tivin00r                               70
006700040910     C                   if        vindta > *blanks
006800000801     C*
006900120521     C                   if        *in70 = *off and
007000000801     C                             (vinflg = *blanks
007100000801     C                              or vinflg = '0'
007200000801     C                              or vinflg = '2')
007300000801     C*
007400000801     C                   clear                   vinmsg
007500000801     C                   eval      vinflg = '1'
007600040910     C*
007700040910     C* Eseguo routine d traduzione
007800120521     C                   EXSR      impvab
007900120521     C                   EXSR      wriVGDSHPM
008000040802     C*
008100010305     C                   endif
008200000905     C*
008300000905     C                   else
008400000905     C                   eval      vinflg = '1'
008500000905     C                   endif
008600000905     C*
008700000905     C  N70              update    tivin000
008800000905     C*
008900120521     C  N70              ENDDO
009000120521     C*
009100990910     C* Se non ci sono record con errori ...
009200120521     C                   if        errore = 0
009300990910     C* ... restituisco esito OK.
009400990921     C                   eval      wrkesito = '0'
009500990910     C                   else
009600010201     C                   if        §ctrokvb > 0
009700990921     C                   eval      wrkesito = '1'
009800000710     C                   else
009900000710     C                   eval      wrkesito = '2'
010000990910     C                   endif
010100000710     C                   endif
010200990910     C*
010300990914     C                   if        %open(tivin00r)
010400990908     C                   close     tivin00r
010500990914     C                   endif
010600990920     C*
010700910830     C                   ENDSR
010800000613     C***
010900990920
011000000801     C*----------------------------------------------------*
011100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
011200000801     C*----------------------------------------------------*
011300010201     C     INZVAR        BEGSR
011400000801     C*
011500040802     C                   MOVEL     '0'           FlgCAS            1
011600150507     C                   CLEAR                   VATNOT_B
011700000801     C*
011800000801     C                   ENDSR
011900000801     C*----------------------------------------------------*
012000040910     C*  IMPOSTAZIONE CAMPI COSTANTI
012100000801     C*----------------------------------------------------*
012200000801     C     DEFCAM        BEGSR
012300000801     C*
012400120521     C                   CLEAR                   fnvabds
012500020619     C* Imposto i valori di default...
012600150507     C                   Z-ADD     0701500       VABCCM
012700150507     C                   Z-ADD     070           VABLNP
012800150507     C                   Z-ADD     300           VABCTR
012900040823     C                   MOVEL     '1'           VABCBO
013000120521     C                   MOVEL     'C'           VABTSP
013100020619     C* ... e poi verifico se sono stati passati come parametri
013200040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
013300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
013400020619     C                   EXSR      CHKNUM
013500020619     C                   IF        PiInt=*on
013600020619     C                   Z-ADD     PiVal         VABCCM
013700020619     C                   ENDIF
013800040506     C                   ENDIF
013900040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
014000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
014100020619     C                   EXSR      CHKNUM
014200020619     C                   IF        PiInt=*on
014300020619     C                   Z-ADD     PiVal         VABLNP
014400040506     C                   ENDIF
014500020619     C                   ENDIF
014600040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
014700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
014800020619     C                   EXSR      CHKNUM
014900020619     C                   IF        PiInt=*on
015000020619     C                   Z-ADD     PiVal         VABCTR
015100040506     C                   ENDIF
015200020619     C                   ENDIF
015300070427     C                   IF        %subst(vlrppt:14:1) <> *blanks
015400070427     C                   EVAL      VABTSP = %subst(vlrppt:14:1)
015500070427     C                   ENDIF
015600120521     C                   IF        %subst(vlrppt:15:1) <> *blanks
015700120521     C                   EVAL      pptCSV = %subst(vlrppt:15:1)
015800120521     C                   ELSE
015900120521     C                   EVAL      pptCSV = ';'
016000120521     C                   ENDIF
016100120521     C                   IF        %subst(vlrppt:16:1) <> *blanks
016200120521     C                   EVAL      pptCHR = %subst(vlrppt:16:1)
016300120521     C                   ELSE
016400120521     C                   EVAL      pptCHR = ' '
016500120521     C                   ENDIF
016600000801     C*
016700000801     C                   ENDSR
016800000801     C*----------------------------------------------------*
016900120521     C*  IMPOSTAZIONE CAMPI DA FLAT FILE
017000000801     C*----------------------------------------------------*
017100120521     C     IMPVAB        BEGSR
017200040910     C*
017300120521     C* Inizializzazioni
017400120521     C                   exsr      inzvar
017500120521     C                   exsr      defcam
017600120521     C*
017700081222     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
017800081222     C                   MOVEL     datcor        VABAAS
017900081222     C                   MOVE      datcor        VABMGS
018000120521     C*
018100150507     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
018200060928     C* == sostituisco caratteri che bloccano POSTEL/piccione ==
018300060928     C     '@':'A'       XLATE     VABRSD        VABRSD
018400060928     C     '§':'S'       XLATE     VABRSD        VABRSD
018500060928     C     '!':' '       XLATE     VABRSD        VABRSD
018600060928     C     ':':' '       XLATE     VABRSD        VABRSD
018700120521     C*
018800150507     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
018900150507     C                   EVAL      VABLOD=%trim(%subst(vindta:112:28))
019000150507     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
019100150507     C                   EVAL      VABCAD=%trim(%subst(vindta:107:5))
019200150507     C                   EVAL      VABRMA=%trim(%subst(vindta:187:5))
019300120521     C*
019400040910     C* Reperisco la provincia dal CAP e dalla località
019500040910     C                   IF        VABCAD <> *blanks AND
019600040910     C                             VABPRD  = *blanks
019700040910     C                   CLEAR                   TISI95DS
019800040910     C                   EVAL      I95TCN = '3'
019900040910     C                   Z-ADD     datcor        I95DAT
020000040910     C                   EVAL      I95CAP = VABCAD
020100040910     C                   EVAL      I95LOC = VABLOD
020200040910     C                   CALL      'TISI95R'
020300040910     C                   PARM                    TISI95DS
020400040910     C                   EVAL      VABPRD = O95PRV
020500040910     C                   ENDIF
020600120521     C*
020700120521     C* VABRMN / VABNSP
020800150507     C                   EVAL      PiStr=%trim(%subst(vindta:187:5))
020900120130     C                   EXSR      CHKNUM
021000120130     C                   IF        PiInt=*on
021100120130     C                   ADD       PiVal         VABRMN
021200120521     C                   ADD       PiVal         VABNSP
021300120130     C                   ELSE
021400120130     C                   ADD       1             errore
021500120130     C                   EVAL      vinmsg = %trimr(vinmsg)
021600120521     C                             + ' ' + 'VABRMN VABNSP'
021700120130     C                   ENDIF
021800120521     C* VABNCL
021900150507     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
022000040910     C                   EXSR      CHKNUM
022100040910     C                   IF        PiInt=*on
022200120521     C                   Z-ADD     PiVal         VABNCL
022300040910     C                   ELSE
022400040910     C                   ADD       1             errore
022500040910     C                   EVAL      vinmsg = %trimr(vinmsg)
022600040910     C                             + ' ' + 'VABNCL'
022700040910     C                   ENDIF
022800120521     C* VABPKB
022900150507     C                   EVAL      PiStr=%trim(%subst(vindta:170:6))
023000040910     C                   EXSR      CHKNUM
023100040910     C                   IF        PiNum=*on
023200150507     C                   EVAL      PiVal=PiVal/1
023300150507     C                   Z-ADD(H)  PiVal         VABPKB                         * gestisco 0 dec
023400040910     C                   ELSE
023500040910     C                   ADD       1             errore
023600040910     C                   EVAL      vinmsg = %trimr(vinmsg)
023700040910     C                             + ' ' + 'VABPKB'
023800040910     C                   ENDIF
023900120521     C* VABCAS
024000150507     C***                IF        %subst(vindta:335:9) <> *zeros AND
024100150507     C***                          %subst(vindta:335:9) <> *blanks
024200150507     C***                EVAL      FlgCAS = '1'
024300150507     C***                EVAL      VABVCA='EUR'
024400150507     C***                EVAL      PiStr=%trim(%subst(vindta:335:9))
024500150507     C***                EXSR      CHKNUM
024600150507     C***                IF        PiNum=*on
024700150507     C***                Z-ADD     PiVal         VABCAS
024800150507     C***                ELSE
024900150507     C***                ADD       1             errore
025000150507     C***                EVAL      vinmsg = %trimr(vinmsg)
025100150507     C***                          + ' ' + 'VABCAS'
025200150507     C***                ENDIF
025300150507     C***                ENDIF
025400120521     C* VABCBO
025500120521     C                   IF        FlgCAS = '1'
025600120521     C                   EVAL      VABCBO='4'
025700120521     C                   ELSE
025800120521     C                   EVAL      VABCBO='1'
025900120521     C                   ENDIF
026000120521     C* VABNOT
026100150507     C***                EVAL      VABNOT=%trim(%subst(vindta:410:40))
026200150507     C* VATNOT_B
026300150507     C                   EVAL      VATNOT_B=%trim(%subst(vindta:92:15))
026400010202     C*
026500010201     C                   ADD       1             §CTROKVB
026600000801     C*
026700000801     C                   ENDSR
026800000801     C*----------------------------------------------------*
026900000801     C*  CONTROLLO NUMERICITA' CAMPI
027000000801     C*----------------------------------------------------*
027100000801     C     CHKNUM        BEGSR
027200081222     C*
027300081222     C                   IF        PiDecChr = *blanks
027400150507     C                   EVAL      PiDecChr = '.'
027500081222     C                   ENDIF
027600081222     C*
027700081222     C                   callp(e)  UBISNUM_Check(PiStr
027800081222     C                                          :PiDecChr
027900081222     C                                          :PiVal
028000081222     C                                          :PiNum
028100081222     C                                          :PiInt)
028200081222     C*
028300081222     C                   IF        %error
028400081222     C                   EVAL      PiInt=*off
028500081222     C                   ENDIF
028600000801     C*
028700000801     C                   ENDSR
028800000801     C***
028900120207
029000120207
029100120207
029200120521      /TITLE Scrittura record in file TIVGD00F (file VAS generico download)
029300120521     C     wriVGDHEAD    BEGSR
029400120521     C*
029500120521     C                   eval      wBufferOut =
029600150507     C                             %trim(pptCHR)+'VABLNP' +%trim(pptCHR)+pptCSV+
029700120521     C                             %trim(pptCHR)+'VABNSP' +%trim(pptCHR)+pptCSV+
029800120521     C                             %trim(pptCHR)+'VABTSP' +%trim(pptCHR)+pptCSV+
029900120521     C                             %trim(pptCHR)+'VABCTR' +%trim(pptCHR)+pptCSV+
030000120521     C                             %trim(pptCHR)+'VABRMN' +%trim(pptCHR)+pptCSV+
030100120521     C                             %trim(pptCHR)+'VABRMA' +%trim(pptCHR)+pptCSV+
030200120521     C                             %trim(pptCHR)+'VABCBO' +%trim(pptCHR)+pptCSV+
030300120521     C                             %trim(pptCHR)+'VABRSD' +%trim(pptCHR)+pptCSV+
030400120521     C                             %trim(pptCHR)+'VABIND' +%trim(pptCHR)+pptCSV+
030500120521     C                             %trim(pptCHR)+'VABLOD' +%trim(pptCHR)+pptCSV+
030600120521     C                             %trim(pptCHR)+'VABPRD' +%trim(pptCHR)+pptCSV+
030700120521     C                             %trim(pptCHR)+'VABCAD' +%trim(pptCHR)+pptCSV+
030800120521     C                             %trim(pptCHR)+'VABPKB' +%trim(pptCHR)+pptCSV+
030900120521     C                             %trim(pptCHR)+'VABNCL' +%trim(pptCHR)+pptCSV+
031000150507     C***                          %trim(pptCHR)+'VABCAS' +%trim(pptCHR)+pptCSV+
031100150507     C***                          %trim(pptCHR)+'VABVCA' +%trim(pptCHR)+pptCSV+
031200150507     C***                          %trim(pptCHR)+'VABNOT' +%trim(pptCHR)+pptCSV+
031300150507     C***                          %trim(pptCHR)+'VABNT2' +%trim(pptCHR)+pptCSV+
031400150507     C                             %trim(pptCHR)+'VABTRC' +%trim(pptCHR)+pptCSV+
031500120613     C                             %trim(pptCHR)+'VABCCM' +%trim(pptCHR)
031600120521     C                   exsr      wriVGD
031700120521     C*
031800120521     C                   ENDSR
031900120521     C*------------------------------------------------------------------------*
032000120521
032100120521
032200120521      /TITLE Scrittura record in file TIVGD00F (file VAS generico download)
032300120521     C     wriVGDSHPM    BEGSR
032400120521     C*
032500120521     C                   eval      wBufferOut =
032600150507     C                             %trim(pptCHR)+%editc(VABLNP:'X')
032700150507     C                                                    +%trim(pptCHR)+pptCSV+
032800120521     C                             %trim(pptCHR)+%editc(VABNSP:'X')
032900120521     C                                                    +%trim(pptCHR)+pptCSV+
033000120521     C                             %trim(pptCHR)+%trim(VABTSP)
033100120521     C                                                    +%trim(pptCHR)+pptCSV+
033200120521     C                             %trim(pptCHR)+%editc(VABCTR:'X')
033300120521     C                                                    +%trim(pptCHR)+pptCSV+
033400120521     C                             %trim(pptCHR)+%editc(VABRMN:'X')
033500120521     C                                                    +%trim(pptCHR)+pptCSV+
033600120521     C                             %trim(pptCHR)+%trim(VABRMA)
033700120521     C                                                    +%trim(pptCHR)+pptCSV+
033800120521     C                             %trim(pptCHR)+%trim(VABCBO)
033900120521     C                                                    +%trim(pptCHR)+pptCSV+
034000120521     C                             %trim(pptCHR)+%trim(VABRSD)
034100120521     C                                                    +%trim(pptCHR)+pptCSV+
034200120521     C                             %trim(pptCHR)+%trim(VABIND)
034300120521     C                                                    +%trim(pptCHR)+pptCSV+
034400120521     C                             %trim(pptCHR)+%trim(VABLOD)
034500120521     C                                                    +%trim(pptCHR)+pptCSV+
034600120521     C                             %trim(pptCHR)+%trim(VABPRD)
034700120521     C                                                    +%trim(pptCHR)+pptCSV+
034800120521     C                             %trim(pptCHR)+%trim(VABCAD)
034900120521     C                                                    +%trim(pptCHR)+pptCSV+
035000120521     C                             %trim(pptCHR)+%editc(VABPKB:'4')
035100120521     C                                                    +%trim(pptCHR)+pptCSV+
035200120521     C                             %trim(pptCHR)+%editc(VABNCL:'4')
035300120521     C                                                    +%trim(pptCHR)+pptCSV+
035400150507     C***                          %trim(pptCHR)+%editc(VABCAS:'4')
035500150507     C***                                                 +%trim(pptCHR)+pptCSV+
035600150507     C***                          %trim(pptCHR)+%trim(VABVCA)
035700150507     C***                                                 +%trim(pptCHR)+pptCSV+
035800150507     C***                          %trim(pptCHR)+%trim(VABNOT)
035900150507     C***                                                 +%trim(pptCHR)+pptCSV+
036000150507     C***                          %trim(pptCHR)+%trim(VABNT2)
036100150507     C***                                                 +%trim(pptCHR)+pptCSV+
036200150507     C                             %trim(pptCHR)+%trim(VATNOT_B)
036300150507     C                                                    +%trim(pptCHR)+pptCSV+
036400120613     C                             %trim(pptCHR)+%editc(VABCCM:'X')
036500120613     C                                                    +%trim(pptCHR)
036600120521     C                   exsr      wriVGD
036700120521     C*
036800120521     C                   ENDSR
036900120521     C*------------------------------------------------------------------------*
037000120521
037100120521
037200120207      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
037300120207     C     wriVGD        BEGSR
037400120207     C*
037500120207     C                   clear                   tivgd000
037600120521     C                   eval      vgdDTA = wBufferOut
037700120207     C                   eval      vgdTIP = 'EB'
037800120207     C                   eval      vgdKSU = vlrKSC
037900120207     C                   eval      vgdTSC = 'WW'
038000120207     C                   eval      vgdDAT = datcor
038100150507     C                   eval      vgdPGM = 'TITV2T9R'
038200120207     C                   write     tivgd000
038300120207     C*
038400120207     C                   ENDSR
038500120207     C*------------------------------------------------------------------------*
038600120207
038700070411
038800990910
038900000613     C     *inzsr        BEGSR
039000990910     C*
039100990910     C     *entry        plist
039200990920     C                   parm                    tivlrds
039300990921     C                   parm      wrkesito      esito
039400000724     C                   parm                    prmlit
039500000710     C                   parm                    prmfir
039600000613     C*
039700000830     C* CALCOLA LA DATA CORRENTE
039800120207     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
039900120207     C                   eval      datcor = %dec(%date() : *iso)
040000000830     C*
040100000613     C                   ENDSR
040200000613     C***
