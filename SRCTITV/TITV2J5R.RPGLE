000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050408     FFIVABWWR  O    E             DISK    usropn
000700120116     Ftitv2j5p  O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900120116     Ftitv2j5ps O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100000313     D*
001200000613     D*------------
001300000613     D* COMANDI
001400000613     D*------------
001500011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001600010330     D*----------------------------------------------------
001700010330     D* DICHIARAZIOINE VARIABILI DI WRK
001800010330     D*----------------------------------------------------
001900010330     D dscmz         e ds                  inz
002000010330     D psds           sds
002100010330     D  procname         *PROC
002200010330     D tivlrds       e ds                  extname(tivlr00f)
002300070502     D tisi95ds      e ds
002400010330     D esito           s              1
002500010330     D prmlit          s             10
002600010330     D prmfir          s             10
002700010330     D wrkesito        s                   like(esito)
002800010330     D wrkdata         s               d
002900010330     D wrkora          s               t
003000010330     D rrnum           s              6  0 INZ(*zeros)
003100010330     D recko           s            150    INZ(*blanks)
003200011113     D depcmd          s            150    INZ(*blanks)
003300010330
003400010330
003500990915     C                   time                    wrkdata
003600990915     C                   time                    wrkora
003700000913     C                   reset                   rrnum
003800990921     C                   reset                   esito
003900990921     C                   reset                   wrkesito
004000010601     C*
004100010601     C                   exsr      opeini
004200010605     C                   exsr      rwvab
004300070502     C*
004400070502     C* Effettuo lancio TISI95 solo x chiusura
004500070502     C                   CLEAR                   TISI95DS
004600070502     C                   EVAL      I95TLA = 'C'
004700070502     C                   CALL      'TISI95R'
004800070502     C                   PARM                    TISI95DS
004900010601     C*
005000010601     C                   seton                                        lr
005100010601
005200010601
005300010601
005400010601
005500010601     C*--------------------------------------------------------
005600010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005700010601     C*--------------------------------------------------------
005800010601     C     PREELA        BEGSR
005900010601     C*
006000010601     C* SE OCCORRE SPEDIRE IN FILIALE
006100010601     C                   if        invfil <> *zeros and
006200010601     C                             flgGiro = '0'
006300010601     C*
006400010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006500010601     C                   eval      flgGiro = '1'
006600000724     C*
006700000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006800000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006900010601     C     invfil        chain     azorg01l
007000000724     C                   if        %found
007100000724     C                   movel(p)  CMD(1)        depcmd
007200020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007300000724     C*
007400000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007500011113     C                   Z-ADD     150           LENGH            15 5
007600000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007700000724     C                   PARM                    depcmd
007800000724     C                   PARM                    LENGH
007900000724     C*
008000000724     C                   endif
008100000616     C*
008200000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008300000616     C                   movel(p)  CMD(2)        depcmd
008400000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008500000616     C*
008600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008700011113     C                   Z-ADD     150           LENGH            15 5
008800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008900000616     C                   PARM                    depcmd
009000000616     C                   PARM                    LENGH
009100000613     C*
009200120116     C                   if        not %open(titv2j5ps)
009300120116     C                   open      titv2j5ps
009400000616     C                   except    testdett
009500000613     C                   endif
009600010601     C*
009700010601     C                   endif
009800010601     C*
009900010601     C                   ENDSR
010000010601     C***
010100010601
010200010601
010300010601
010400010601     C*--------------------------------------------------------
010500010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010600010601     C*--------------------------------------------------------
010700010601     C     ENDELA        BEGSR
010800990915     C*
010900000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011000000613     C*
011100120116     C                   if        %open(titv2j5ps)
011200000616     C                   except    findett
011300120116     C                   close     titv2j5ps
011400000613     C                   endif
011500000616     C*
011600000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011700011113     C                   Z-ADD     150           LENGH            15 5
011800010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
011900000616     C                   PARM                    CMD(3)
012000000616     C                   PARM                    LENGH
012100000616     C*
012200010601     C                   ENDSR
012300010601     C***
012400010601
012500010601
012600000613
012700000613     C*--------------------------------------------------------
012800000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
012900000613     C*--------------------------------------------------------
013000000621     C     STPR          BEGSR
013100000613     C*
013200120116     C                   if        not %open(titv2j5p)
013300120116     C                   open      titv2j5p
013400990915     C                   endif
013500990915     C*
013600990915     C                   except    riepilogo
013700990915     C*
013800120116     C                   if        %open(titv2j5p)
013900120116     C                   close     titv2j5p
014000990914     C                   endif
014100000613     C*
014200000613     C                   ENDSR
014300000613     C***
014400000613
014500010601
014600010601
014700010330     C*--------------------------------------------------------
014800050408     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWR              *
014900010330     C*--------------------------------------------------------
015000010605     C     RWVAB         BEGSR
015100050408     C*
015200010330     C                   if        not %open(tivin00r)
015300010330     C                   open      tivin00r
015400010330     C                   endif
015500050408     C                   if        not %open(fivabwwr)
015600050408     C                   open      fivabwwr
015700010330     C                   endif
015800010330     C*
015900010604     C                   clear                   §CTROK
016000010604     C                   clear                   §CTRMO
016100010604     C                   clear                   §CTRNO
016200010330     C*
016300010330     C                   DO        *HIVAL
016400010330     C*
016500010330     C                   READ      tivin00r                               70
016600010618     C*
016700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
016800010618     C                   if        vinflg = '1'
016900010618     C                   eval      flgOk = '1'
017000010618     C                   endif
017100010618     C*
017200010330     C                   if        vindta > *blanks
017300010330     C                   add       1             rrnum
017400010330     C*
017500010601     C                   if        *in70 = *off and
017600010330     C                             (vinflg = *blanks
017700010330     C                              or vinflg = '0'
017800010330     C                              or vinflg = '2')
017900010330     C*
018000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018100010711     C                   if        vinflg = *blanks or vinflg = '0'
018200010711     C                   clear                   vinmsg
018300010711     C                   endif
018400070601     C*
018500070601     C                   exsr      inzvar
018600070601     C                   exsr      defcam
018700070601     C                   exsr      impvab
018800010601     C*
018900010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019000010605     C                   eval      depfil = VABLNP
019100010601     C                   exsr      repfil
019200010601     C                   if        depfil = invfil
019300021025     C                   if        vlrpoi = 999
019400021025     C                   MOVE(P)   invfil        VABFGS
019500021025     C                   else
019600021025     C                   MOVE(P)   vlrpoi        VABFGS
019700021025     C                   endif
019800010601     C*
019900010601     C                   exsr      PREELA
020000010601     C*
020100010604     C* Ebbene...
020200010604     C*
020300010604     C  N31              ADD       1             §CTROK            7 0
020400010604     C   32              ADD       1             §CTRMO            7 0
020500010604     C   31              ADD       1             §CTRNO            7 0
020600010604     C*
020700071120     C                   exsr      wrivab                                       => scrivo VAB
020800071120     C*
020900020722     C                   endif
021000020722     C*
021100010604     C                   if        *in31 = *off and
021200010604     C                             *in32 = *off
021300010604     C                   eval      vinflg = '1'
021400010604     C                   else
021500010604     C                   eval      recko = vindta
021600010604     C                   except    rigadett
021700010604     C                   eval      vinflg = '2'
021800010604     C                   endif
021900010604     C                   endif
022000010604     C*
022100010330     C                   else
022200010330     C                   eval      vinflg = '1'
022300010330     C                   endif
022400010601     C*
022500010601     C  N70              update    tivin000
022600010330     C*
022700010330     C  N70              ENDdo
022800010601     C*
022900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023000010601     C                   if        cntNonEl = *zeros or
023100010601     C                             flgMulti = '0'
023200010330     C* Se non ci sono record con errori ...
023300010601     C                   if        §ctrno = 0 and
023400010604     C                             §ctrmo = 0 and
023500010601     C                             flgStato <> '2'
023600010330     C* ... restituisco esito OK.
023700010330     C                   eval      wrkesito = '0'
023800010330     C                   else
023900010330     C                   if        §ctrok > 0
024000010330     C                   eval      wrkesito = '1'
024100010330     C                   else
024200010615     C                   if        flgOk = '0'
024300010615     C                   eval      wrkesito = '2'
024400010615     C                   else
024500010615     C                   eval      wrkesito = '6'
024600010615     C                   endif
024700010330     C                   endif
024800010330     C                   endif
024900010601     C                   else
025000010601     C                   eval      wrkesito = '9'
025100010601     C                   endif
025200010330     C*
025300010330     C                   if        %open(tivin00r)
025400010330     C                   close     tivin00r
025500010330     C                   endif
025600050408     C                   if        %open(fivabwwr)
025700050408     C                   close     fivabwwr
025800010330     C                   endif
025900010601     C*
026000010601     C                   if        vlrpoi <> 999
026100010601     C                   eval      invfil = vlrpoi
026200010601     C                   endif
026300010330     C*
026400010330     C                   if        §ctrok > 0
026500010601     C                             and invfil > *zeros
026600010330     C                   exsr      invio
026700010330     C                   endif
026800010601     C*
026900010618     C                   if        flgGiro = '1'
027000010601     C                   exsr      endela
027100010618     C                   endif
027200010330     C*
027300010330     C                   ENDSR
027400010330     C***
027500070601
027600070601     C*----------------------------------------------------*
027700070601     C*  SCARICAMENTO BUFFER RECORDS VAB
027800070601     C*----------------------------------------------------*
027900070601     C     WRIVAB        BEGSR
028000070601     C*
028100070601     C  N31              WRITE     FIVAB000
028200070601     C*
028300070601     C                   ENDSR
028400010601
028500010601
028600010601
028700010330     C*----------------------------------------------------*
028800020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028900010330     C*----------------------------------------------------*
029000010330     C     INZVAR        BEGSR
029100010330     C*
029200020204     C                   Z-ADD     *zeros        Num5_0            5 0
029300020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
029400020322     C                   MOVEL     '0'           FlgCAS            1
029500010330     C*
029600010330     C                   ENDSR
029700010330     C*----------------------------------------------------*
029800020722     C*  IMPOSTAZIONE CAMPI COSTANTI
029900010330     C*----------------------------------------------------*
030000010330     C     DEFCAM        BEGSR
030100010330     C*
030200020204     C* Inizializzo il buffer del record da scrivere
030300050408     C                   CLEAR                   FIVAB000
030400020204     C* Imposto i valori di default...
030500120116     C                   EVAL      VABCCM = 2751361
030600120116     C                   EVAL      VABLNP = 001
030700080311     C                   EVAL      VABCTR = 100
030800080311     C                   EVAL      VABCTM = '7Q'
030900070208     C                   EVAL      VABCBO = '1'
031000020204     C* ... e poi verifico se sono stati passati come parametri
031100020204     C                   IF        vlrppt > *blanks
031200040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
031300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031400020204     C                   EXSR      CHKNUM
031500020204     C                   IF        PiInt=*on
031600020204     C                   Z-ADD     PiVal         VABCCM
031700020204     C                   ENDIF
031800040301     C                   ENDIF
031900040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
032000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032100020204     C                   EXSR      CHKNUM
032200020204     C                   IF        PiInt=*on
032300020204     C                   Z-ADD     PiVal         VABLNP
032400020204     C                   ENDIF
032500040301     C                   ENDIF
032600040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
032700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032800020204     C                   EXSR      CHKNUM
032900020204     C                   IF        PiInt=*on
033000020204     C                   Z-ADD     PiVal         VABCTR
033100040301     C                   ENDIF
033200020204     C                   ENDIF
033300020204     C                   ENDIF
033400020204     C*
033500010330     C                   ENDSR
033600010607     C*----------------------------------------------------*
033700050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033800010607     C*----------------------------------------------------*
033900010607     C     IMPVAB        BEGSR
034000010607     C*
034100010607     C                   SETOFF                                       3132
034200010607     C*
034300010607     C* Reperimento campi ALFA
034400010607     C*
034500010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
034600070925     C                   EVAL      VABRSD=%trim(%subst(vindta:50:35))
034700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034900020117     C* ==
035000080311     C                   EVAL      VABRD2=%trim(%subst(vindta:407:20))
035100070925     C                   EVAL      VABIND=%trim(%subst(vindta:85:35))
035200070925     C                   EVAL      VABLOD=%trim(%subst(vindta:120:25))
035300070925     C                   EVAL      VABPRD=%trim(%subst(vindta:154:2))
035400080311     C***                EVAL      VABNZD=%trim(%subst(vindta:156:3))
035500070925     C                   EVAL      VABRMA=%trim(%subst(vindta:35:6))
035600080311     C                   EVAL      VABNOT=%trim(%subst(vindta:306:35))
035700080311     C                   EVAL      VABNT2=%trim(%subst(vindta:427:35))
035800080311     C                   IF        %subst(vindta:210:1) = 'M'
035900080311     C                   EVAL      VABTIC='BM'
036000080311     C                   ENDIF
036100080311     C                   IF        %subst(vindta:210:1) = 'N'
036200080311     C                   EVAL      VABTIC='TM'
036300080311     C                   ENDIF
036400100527     C                   IF        %subst(vindta:210:1) = 'R'
036500100527     C                   EVAL      VABTIC='CM'
036600080311     C                   ENDIF
036700080311     C                   IF        %subst(vindta:227:4) = *blanks
036800080311     C                   EVAL      VABVCA='EUR'
036900120123     C                   EVAL      VABVAS='EUR'
037000080311     C                   ENDIF
037100080311     C                   IF        %subst(vindta:231:1) = 'I'
037200080311     C                   EVAL      VABTC1='A'
037300080311     C                   ENDIF
037400080311     C                   IF        %subst(vindta:231:1) = '2'
037500080311     C                   EVAL      VABTC2='P'
037600080311     C                   ENDIF
037700080311     C                   IF        %subst(vindta:231:1) = '1'
037800080311     C                   EVAL      VABTC2='S'
037900080311     C                   ENDIF
038000060510     C*
038100060510     C* Reperimento campi NUMERICI
038200060510     C                   MOVEL     DATCOR        VABAAS
038300060510     C                   MOVE      DATCOR        VABMGS
038400080311     C* CCM
038500120116     C                   IF        %trim(%subst(vindta:23:6)) = 'SNATT'
038600120116     C                   EVAL      VABCCM=2751361
038700080311     C                   ENDIF
038800080311     C* DCR
038900080311     C                   EVAL      PiStr=%trim(%subst(vindta:11:8))
039000080311     C                   EXSR      CHKNUM
039100080311     C                   IF        PiInt=*on
039200080311     C                   Z-ADD     PiVal         VABDCR
039300080311     C                   ELSE
039400100527     C                   Z-ADD     *zeros        VABDCR
039500080311     C                   EVAL      vinmsg = %trimr(vinmsg)
039600080311     C                             + ' ' + 'VABDCR'
039700080311     C                   ENDIF
039800060710     C* NSP/RMN
039900080311     C                   EVAL      PiStr=%trim(%subst(vindta:35:15))
040000060510     C                   EXSR      CHKNUM
040100060510     C                   IF        PiInt=*on
040200060510     C                   Z-ADD     PiVal         VABNSP
040300060510     C                   Z-ADD     PiVal         VABRMN
040400060404     C                   ELSE
040500070502     C                   SETON                                        31
040600060510     C                   Z-ADD     *zeros        VABNSP
040700060510     C                   Z-ADD     1             VABRMN
040800060404     C                   EVAL      vinmsg = %trimr(vinmsg)
040900060510     C                             + ' ' + 'VABNSP VABRMN'
041000060404     C                   ENDIF
041100040420     C* CAD
041200080311     C                   EVAL      PiStr=%trim(%subst(vindta:145:9))
041300010607     C                   EXSR      CHKNUM
041400010607     C                   IF        PiInt=*on
041500020204     C                   Z-ADD     PiVal         Num5_0
041600020204     C                   MOVEL(p)  Num5_0        VABCAD
041700010607     C                   ELSE
041800010607     C                   SETON                                        32
041900020204     C                   EVAL      VABCAD = *zeros
042000010607     C                   EVAL      vinmsg = %trimr(vinmsg)
042100020204     C                             + ' ' + 'VABCAD'
042200060316     C                   ENDIF
042300070502     C* Reperisco la provincia dal CAP e dalla località
042400070502     C                   IF        VABCAD <> *blanks AND
042500070502     C                             VABPRD  = *blanks
042600070502     C                   CLEAR                   TISI95DS
042700070502     C                   EVAL      I95TCN = '3'
042800070502     C                   Z-ADD     datcor        I95DAT
042900070502     C                   EVAL      I95CAP = VABCAD
043000070502     C                   EVAL      I95LOC = VABLOD
043100070502     C                   EVAL      I95NAR = VABNZD
043200070502     C                   CALL      'TISI95R'
043300070502     C                   PARM                    TISI95DS
043400070502     C                   EVAL      VABPRD = O95PRV
043500070502     C                   ENDIF
043600040420     C* NCL
043700070925     C                   EVAL      PiStr=%trim(%subst(vindta:164:5))
043800010607     C                   EXSR      CHKNUM
043900010607     C                   IF        PiInt=*on
044000070606     C                   Z-ADD     PiVal         VABNCL
044100010607     C                   ELSE
044200010607     C                   SETON                                        32
044300010607     C                   Z-ADD     *zeros        VABNCL
044400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
044500010607     C                             + ' ' + 'VABNCL'
044600010607     C                   ENDIF
044700040420     C* PKB
044800070925     C                   EVAL      PiStr=%trim(%subst(vindta:159:5))
044900070208     C                   EVAL      PiDecChr = '.'
045000010607     C                   EXSR      CHKNUM
045100010607     C                   IF        PiNum=*on
045200070925     C                   Z-ADD(H)  PiVal         VABPKB
045300010607     C                   ELSE
045400010607     C                   SETON                                        32
045500010607     C                   Z-ADD     *zeros        VABPKB
045600010607     C                   EVAL      vinmsg = %trimr(vinmsg)
045700010607     C                             + ' ' + 'VABPKB'
045800010607     C                   ENDIF
045900070925     C* VLB
046000070925     C                   EVAL      PiStr=%trim(%subst(vindta:184:6))
046100070925     C                   EVAL      PiDecChr = '.'
046200070925     C                   EXSR      CHKNUM
046300070925     C                   IF        PiNum=*on
046400070925     C                   EVAL(H)   PiVal = PiVal / 1000                         * gest. 3 decimali
046500070925     C                   Z-ADD     PiVal         VABVLB
046600070925     C                   ELSE
046700070925     C                   SETON                                        32
046800070925     C                   Z-ADD     *zeros        VABVLB
046900070925     C                   EVAL      vinmsg = %trimr(vinmsg)
047000070925     C                             + ' ' + 'VABVLB'
047100070925     C                   ENDIF
047200080311     C* CAS
047300080311     C                   IF        %subst(vindta:196:14) <> *blanks AND
047400080311     C                             %subst(vindta:196:14) <> *zeros
047500080311     C                   EVAL      FlgCAS = '1'
047600080311     C                   EVAL      PiStr=%trim(%subst(vindta:196:14))
047700080311     C                   EVAL      PiDecChr = '.'
047800080311     C                   EXSR      CHKNUM
047900080311     C                   IF        PiNum=*on
048000080311     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
048100080311     C                   Z-ADD     PiVal         VABCAS
048200080311     C                   ELSE
048300080311     C                   SETON                                        32
048400120123     C                   Z-ADD     *zeros        VABCAS
048500080311     C                   EVAL      vinmsg = %trimr(vinmsg)
048600080311     C                             + ' ' + 'VABCAS'
048700080311     C                   ENDIF
048800080311     C                   ENDIF
048900120123     C* IAS
049000120123     C                   IF        %subst(vindta:231:1) = 'C'       AND
049100120123     C                             %subst(vindta:232:14) <> *blanks AND
049200120123     C                             %subst(vindta:232:14) <> *zeros
049300120123     C                   EVAL      PiStr=%trim(%subst(vindta:232:14))
049400120123     C                   EXSR      CHKNUM
049500120123     C                   IF        PiNum=*on
049600120123     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
049700120123     C                   Z-ADD     PiVal         VABIAS
049800120123     C                   ELSE
049900120123     C                   SETON                                        32
050000120123     C                   Z-ADD     *zeros        VABIAS
050100120123     C                   EVAL      vinmsg = %trimr(vinmsg)
050200120123     C                             + ' ' + 'VABIAS'
050300120123     C                   ENDIF
050400120123     C                   ENDIF
050500020322     C*
050600020322     C* Considerazioni finali su CBO/CAS
050700050908     C                   IF        FlgCAS = '1'
050800040713     C                   IF        VABCBO = '1'
050900020322     C                   EVAL      VABCBO = '4'
051000040713     C                   ENDIF
051100040713     C                   IF        VABCBO = '2'
051200040713     C                   EVAL      VABCBO = '6'
051300040713     C                   ENDIF
051400020322     C                   ENDIF
051500020204     C*
051600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
051700020204     C                   EXSR      CHKIMPDIV
051800020204     C*
051900010607     C                   ENDSR
052000010607     C*----------------------------------------------------*
052100010601
052200020204
052300020204     C*----------------------------------------------------*
052400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
052500020204     C*----------------------------------------------------*
052600020204     C     CHKIMPDIV     BEGSR
052700020204     C*
052800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
052900020204     C                   Z-ADD     *zeros        wrkDec            9 9
053000020204     C*
053100020204     C* Come prima cosa effettuo considerazioni sulla divisa
053200020204     C                   IF        vabIAS > *zeros
053300020204     C                   IF        vabVAS <> 'EUR'
053400020204     C                   EVAL      vabVAS =  'ITL'
053500020204     C                   ENDIF
053600020204     C                   ENDIF
053700020204     C*
053800020204     C                   IF        vabCAS > *zeros
053900020204     C                   IF        vabVCA <> 'EUR'
054000020204     C                   EVAL      vabVCA =  'ITL'
054100020204     C                   ENDIF
054200020204     C                   ENDIF
054300020204     C*
054400020204     C                   IF        vabVMD > *zeros
054500020321     C                   IF        vabVAD <> 'EUR'
054600020204     C                   EVAL      vabVAD =  'ITL'
054700020204     C                   ENDIF
054800020204     C                   ENDIF
054900020204     C*
055000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
055100020204     C                   Z-ADD     vabIAS        wrkDec
055200020204     C                   IF        wrkDec > *zeros
055300020204     C                   IF        vabVAS = 'ITL'
055400020204     C                   EVAL      vabIAS = *zeros
055500020204     C                   ENDIF
055600020204     C                   ENDIF
055700020204     C*
055800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
055900020204     C                   Z-ADD     vabCAS        wrkDec
056000020204     C                   IF        wrkDec > *zeros
056100020204     C                   IF        vabVCA = 'ITL'
056200020204     C                   EVAL      vabCAS = *zeros
056300020204     C                   ENDIF
056400020204     C                   ENDIF
056500020204     C*
056600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
056700020204     C                   Z-ADD     vabVMD        wrkDec
056800020204     C                   IF        wrkDec > *zeros
056900020204     C                   IF        vabVAD = 'ITL'
057000020204     C                   EVAL      vabVMD = *zeros
057100020204     C                   ENDIF
057200020204     C                   ENDIF
057300020204     C*
057400020204     C                   ENDSR
057500020204     C***
057600020204
057700010330
057800010330
057900010330     C*----------------------------------------------------*
058000010330     C*  CONTROLLO NUMERICITA' CAMPI
058100010330     C*----------------------------------------------------*
058200010330     C     CHKNUM        BEGSR
058300010330     C*
058400010606     C                   IF        PiDecChr = *blanks
058500070502     C                   EVAL      PiDecChr = '.'
058600010606     C                   ENDIF
058700010606     C*
058800010606     C                   CALL(e)   'ISNUMERIC'
058900010330     C                   PARM                    PiStr            30
059000010606     C                   PARM                    PiDecChr          1
059100010330     C                   PARM      *ZEROS        PiVal            30 9
059200010330     C                   PARM      '0'           PiInt             1
059300010330     C                   PARM      '0'           PiNum             1
059400010330     C                   IF        %error
059500010606     C                   EVAL      PiNum=*off
059600010330     C                   ENDIF
059700010330     C*
059800010330     C                   ENDSR
059900010330     C***
060000010330
060100010601
060200010601
060300010601
060400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
060500010601     C     repfil        BEGSR
060600010601     C*
060700010601     C                   if        invfil = *zeros and
060800010601     C                             depfil > *zeros and
060900010629     C                             (vinflg = *blanks or
061000010629     C                              vinflg = *zeros)
061100010601     C
061200010601     C                   eval      invfil = depfil
061300010601     C                   endif
061400010601     C*
061500010601     C                   if        depfil <> invfil and
061600010601     C                             invfil > *zeros
061700010601     C                   eval      flgMulti = '1'
061800010601     C                   if        vinflg = *blanks
061900010601     C                   add       1             cntNonEl
062000010601     C                   endif
062100010601     C                   endif
062200010601     C*
062300010601     C                   if        vinflg = '2'
062400010601     C                   eval      flgStato = '2'
062500010601     C                   endif
062600010601     C*
062700010601     C                   ENDSR
062800010601     C***
062900010601
063000010601
063100010601
063200010330
063300010330
063400010330
063500990920      /TITLE Invio dei dati al punto operativo.
063600000613     C     invio         BEGSR
063700990920     C*
063800990920     C                   reset                   dscmz
063900010601     C                   move      invfil        cmzdst
064000990920     C                   eval      cmzfld = vlrfou
064100990920     C                   eval      cmzmbd = vlrhdl
064200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
064300000710     C                   if        prmfir = *blanks
064400050408     C                   eval      cmzfla = 'FIVAB00F'
064500050408     C                   eval      cmzmba = 'FIVAB00F'
064600000710     C                   else
064700000710     C                   eval      cmzfla = prmfir
064800000710     C                   eval      cmzmba = prmfir
064900000710     C                   endif
065000990920     C                   eval      cmznrr = *zeros
065100990920     C                   move      §ctrok        cmznrr
065200021018     C                   eval      cmzlba = vlrfl1
065300990920     C                   call(e)   'TIS711C'
065400990920     C                   parm                    dscmz
065500990921     C                   parm      *blanks       esito
065600990920     C                   if        %error
065700990920     C                             or cmzerr = '1'
065800990921     C                             or esito  = '1'
065900000710     C                   eval      wrkesito = '3'
066000990920     C                   endif
066100990920     C*
066200000613     C                   ENDSR
066300990910
066400010601
066500010601
066600010601
066700010601
066800010601      /TITLE Invio dei dati al punto operativo.
066900010601     C     opeini        BEGSR
067000010601     C*
067100010601     C* Inizializzo flag e contatori operativi
067200010601     C                   movel     '0'           flgGiro           1
067300010601     C                   movel     '0'           flgMulti          1
067400010601     C                   movel     '1'           flgStato          1
067500010615     C                   movel     '0'           flgOk             1
067600010601     C                   z-add     *zeros        cntNonEl         10 0
067700010601     C                   z-add     *zeros        depfil            3 0
067800010601     C                   z-add     *zeros        invfil            3 0
067900010601     C*
068000010601     C                   ENDSR
068100010601     C***
068200010601
068300010601
068400010601
068500010330
068600010330
068700000613     C     *inzsr        BEGSR
068800990910     C*
068900990910     C     *entry        plist
069000990920     C                   parm                    tivlrds
069100990921     C                   parm      wrkesito      esito
069200000724     C                   parm                    prmlit
069300000710     C                   parm                    prmfir
069400010330     C*
069500010330     C* CALCOLA LA DATA CORRENTE
069600120116     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
069700120116     C                   eval      datcor = %dec(%date() : *ISO)
069800000613     C*
069900000613     C                   ENDSR
070000000613     C***
070100990908
070200120116     Otitv2j5p  E            riepilogo         2
070300990915     O                                              'Upload via Internet'
070400990915     O                                           +1 'Traduzione TIVIN00R -'
070500050408     O                                           +1 'FIVABWWR'
070600990915     O                                           +1 'Testate bolle'
070700990915     O          E            riepilogo   2
070800990915     O                       wrkdata
070900990915     O                       wrkora              +1
071000990915     O                       procname            +1
071100990915     O          E            riepilogo   2
071200990915     O                                              'Cliente..................:'
071300010605     O                       VABCCM        z     +1
071400990915     O          E            riepilogo   2
071500990920     O                                              'Riferimento Strategi.....:'
071600990920     O                       vlrhdl              +1
071700990915     O          E            riepilogo   2
071800990915     O                                              'Giusti...................:'
071900971022     O                       §CTROK        2   +  1
072000990915     O          E            riepilogo   2
072100990915     O                                              'Sbagliati e corretti.....:'
072200971022     O                       §CTRMO        2   +  1
072300990915     O          E            riepilogo   2
072400990915     O                                              'Sbagliati e scartati.....:'
072500971022     O                       §CTRNO        2   +  1
072600000613
072700120116     Otitv2j5ps E            testdett          2
072800000613     O                                              'Upload via Internet'
072900000613     O                                           +1 'Traduzione TIVIN00R -'
073000050408     O                                           +1 'FIVABWWR'
073100000613     O                                           +1 'Testate bolle'
073200000616     O          E            testdett    3
073300000613     O                                           +2 'N° rec'
073400000613     O                                           +3 'Anteprima contenuto'
073500000616     O          E            rigadett    2
073600000613     O                       rrnum               +2
073700000621     O                       recko               +3
073800000616     O          E            findett     2
073900000613     O                       wrkdata
074000000613     O                       wrkora              +1
074100000613     O                       procname            +1
074200000616     O          E            findett     2
074300000613     O                                              'Cliente..................:'
074400010605     O                       VABCCM        z     +1
074500000616     O          E            findett     2
074600000613     O                                              'Riferimento Strategi.....:'
074700000613     O                       vlrhdl              +1
074800000616     O          E            findett     2
074900000613     O                                              'Giusti...................:'
075000000613     O                       §CTROK        2   +  1
075100000616     O          E            findett     2
075200000613     O                                              'Sbagliati e corretti.....:'
075300000613     O                       §CTRMO        2   +  1
075400000616     O          E            findett     2
075500000613     O                                              'Sbagliati e scartati.....:'
075600000613     O                       §CTRNO        2   +  1
075700000613** CMD - COMANDI CL
075800120116OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV2J5P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
075900120116OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV2J5PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
076000120116DLTOVR FILE(TITV2J5P TITV2J5PS) LVL(*)
076100010606
076200000613
