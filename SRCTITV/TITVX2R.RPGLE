000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050408     FFIVABWWR  O    E             DISK    usropn
000700070504     Ftitvx2p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900070504     Ftitvx2ps  O    f  198        PRINTER usropn
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
003300070601     D curspe          s             15    INZ(*blanks)
003400070601     D depspe          s             15    INZ(*blanks)
003500010330     D*------------------
003600010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003700010330     D*------------------
003800010330     D WLBDA8          DS                  INZ
003900010330     D  G08DAT                 1      8  0
004000010330     D  G08INV                 9     16  0
004100010330     D  G08ERR                17     17
004200010330     D  G08TGI                18     22  0
004300010330
004400010330
004500990915     C                   time                    wrkdata
004600990915     C                   time                    wrkora
004700000913     C                   reset                   rrnum
004800990921     C                   reset                   esito
004900990921     C                   reset                   wrkesito
005000010601     C*
005100010601     C                   exsr      opeini
005200010605     C                   exsr      rwvab
005300070502     C*
005400070502     C* Effettuo lancio TISI95 solo x chiusura
005500070502     C                   CLEAR                   TISI95DS
005600070502     C                   EVAL      I95TLA = 'C'
005700070502     C                   CALL      'TISI95R'
005800070502     C                   PARM                    TISI95DS
005900010601     C*
006000010601     C                   seton                                        lr
006100010601
006200010601
006300010601
006400010601
006500010601     C*--------------------------------------------------------
006600010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006700010601     C*--------------------------------------------------------
006800010601     C     PREELA        BEGSR
006900010601     C*
007000010601     C* SE OCCORRE SPEDIRE IN FILIALE
007100010601     C                   if        invfil <> *zeros and
007200010601     C                             flgGiro = '0'
007300010601     C*
007400010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007500010601     C                   eval      flgGiro = '1'
007600000724     C*
007700000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007800000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007900010601     C     invfil        chain     azorg01l
008000000724     C                   if        %found
008100000724     C                   movel(p)  CMD(1)        depcmd
008200020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008300000724     C*
008400000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008500011113     C                   Z-ADD     150           LENGH            15 5
008600000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008700000724     C                   PARM                    depcmd
008800000724     C                   PARM                    LENGH
008900000724     C*
009000000724     C                   endif
009100000616     C*
009200000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009300000616     C                   movel(p)  CMD(2)        depcmd
009400000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009500000616     C*
009600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009700011113     C                   Z-ADD     150           LENGH            15 5
009800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009900000616     C                   PARM                    depcmd
010000000616     C                   PARM                    LENGH
010100000613     C*
010200070504     C                   if        not %open(titvx2ps)
010300070504     C                   open      titvx2ps
010400000616     C                   except    testdett
010500000613     C                   endif
010600010601     C*
010700010601     C                   endif
010800010601     C*
010900010601     C                   ENDSR
011000010601     C***
011100010601
011200010601
011300010601
011400010601     C*--------------------------------------------------------
011500010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011600010601     C*--------------------------------------------------------
011700010601     C     ENDELA        BEGSR
011800990915     C*
011900000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012000000613     C*
012100070504     C                   if        %open(titvx2ps)
012200000616     C                   except    findett
012300070504     C                   close     titvx2ps
012400000613     C                   endif
012500000616     C*
012600000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012700011113     C                   Z-ADD     150           LENGH            15 5
012800010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012900000616     C                   PARM                    CMD(3)
013000000616     C                   PARM                    LENGH
013100000616     C*
013200010601     C                   ENDSR
013300010601     C***
013400010601
013500010601
013600000613
013700000613     C*--------------------------------------------------------
013800000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013900000613     C*--------------------------------------------------------
014000000621     C     STPR          BEGSR
014100000613     C*
014200070504     C                   if        not %open(titvx2p)
014300070504     C                   open      titvx2p
014400990915     C                   endif
014500990915     C*
014600990915     C                   except    riepilogo
014700990915     C*
014800070504     C                   if        %open(titvx2p)
014900070504     C                   close     titvx2p
015000990914     C                   endif
015100000613     C*
015200000613     C                   ENDSR
015300000613     C***
015400000613
015500010601
015600010601
015700010330     C*--------------------------------------------------------
015800050408     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWR              *
015900010330     C*--------------------------------------------------------
016000010605     C     RWVAB         BEGSR
016100050408     C*
016200010330     C                   if        not %open(tivin00r)
016300010330     C                   open      tivin00r
016400010330     C                   endif
016500050408     C                   if        not %open(fivabwwr)
016600050408     C                   open      fivabwwr
016700010330     C                   endif
016800010330     C*
016900010604     C                   clear                   §CTROK
017000010604     C                   clear                   §CTRMO
017100010604     C                   clear                   §CTRNO
017200010330     C*
017300010330     C                   DO        *HIVAL
017400010330     C*
017500010330     C                   READ      tivin00r                               70
017600010618     C*
017700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017800010618     C                   if        vinflg = '1'
017900010618     C                   eval      flgOk = '1'
018000010618     C                   endif
018100010618     C*
018200010330     C                   if        vindta > *blanks
018300010330     C                   add       1             rrnum
018400010330     C*
018500010601     C                   if        *in70 = *off and
018600010330     C                             (vinflg = *blanks
018700010330     C                              or vinflg = '0'
018800010330     C                              or vinflg = '2')
018900010330     C*
019000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019100010711     C                   if        vinflg = *blanks or vinflg = '0'
019200010711     C                   clear                   vinmsg
019300010711     C                   endif
019400070601     C*
019500070601     C* Determino il numero di Spedizione e a rottura eseguo operazioni
019600070601     C                   EVAL      PiStr=%trim(%subst(vindta:80:15))
019700070601     C                   MOVEL(p)  PiStr         curspe
019800070601     C*
019900070601     C                   if        depspe = *blanks                             => 1° giro
020000070601     C                   eval      depspe = curspe                              => memorizz. spediz
020100070601     C                   exsr      inzvar
020200070601     C                   exsr      defcam
020300070601     C                   exsr      impvab
020400070601     C                   else
020500070601     C                   if        curspe <> depspe                             => rottura di spediz
020600070601     C                   eval      depspe = curspe                              => memorizz. spediz
020700070601     C                   exsr      wrivab                                       => scrivo VAB
020800070618     C                   exsr      inzvar
020900070601     C                   exsr      defcam
021000070601     C                   exsr      impvab                                       => carico VAB
021100070601     C                   else                                                   => x stessa spediz
021200070601     C                   exsr      impvab                                       => carico VAB
021300070601     C                   endif
021400070601     C                   endif
021500010601     C*
021600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021700010605     C                   eval      depfil = VABLNP
021800010601     C                   exsr      repfil
021900010601     C                   if        depfil = invfil
022000021025     C                   if        vlrpoi = 999
022100021025     C                   MOVE(P)   invfil        VABFGS
022200021025     C                   else
022300021025     C                   MOVE(P)   vlrpoi        VABFGS
022400021025     C                   endif
022500010601     C*
022600010601     C                   exsr      PREELA
022700010601     C*
022800010604     C* Ebbene...
022900010604     C*
023000010604     C  N31              ADD       1             §CTROK            7 0
023100010604     C   32              ADD       1             §CTRMO            7 0
023200010604     C   31              ADD       1             §CTRNO            7 0
023300010604     C*
023400020722     C                   endif
023500020722     C*
023600010604     C                   if        *in31 = *off and
023700010604     C                             *in32 = *off
023800010604     C                   eval      vinflg = '1'
023900010604     C                   else
024000010604     C                   eval      recko = vindta
024100010604     C                   except    rigadett
024200010604     C                   eval      vinflg = '2'
024300010604     C                   endif
024400010604     C                   endif
024500010604     C*
024600010330     C                   else
024700010330     C                   eval      vinflg = '1'
024800010330     C                   endif
024900010601     C*
025000010601     C  N70              update    tivin000
025100010330     C*
025200010330     C  N70              ENDdo
025300070601     C*
025400070601     C* Scarico i VAB rimasti "in sospeso"
025500070601     C                   exsr      wrivab
025600010601     C*
025700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
025800010601     C                   if        cntNonEl = *zeros or
025900010601     C                             flgMulti = '0'
026000010330     C* Se non ci sono record con errori ...
026100010601     C                   if        §ctrno = 0 and
026200010604     C                             §ctrmo = 0 and
026300010601     C                             flgStato <> '2'
026400010330     C* ... restituisco esito OK.
026500010330     C                   eval      wrkesito = '0'
026600010330     C                   else
026700010330     C                   if        §ctrok > 0
026800010330     C                   eval      wrkesito = '1'
026900010330     C                   else
027000010615     C                   if        flgOk = '0'
027100010615     C                   eval      wrkesito = '2'
027200010615     C                   else
027300010615     C                   eval      wrkesito = '6'
027400010615     C                   endif
027500010330     C                   endif
027600010330     C                   endif
027700010601     C                   else
027800010601     C                   eval      wrkesito = '9'
027900010601     C                   endif
028000010330     C*
028100010330     C                   if        %open(tivin00r)
028200010330     C                   close     tivin00r
028300010330     C                   endif
028400050408     C                   if        %open(fivabwwr)
028500050408     C                   close     fivabwwr
028600010330     C                   endif
028700010601     C*
028800010601     C                   if        vlrpoi <> 999
028900010601     C                   eval      invfil = vlrpoi
029000010601     C                   endif
029100010330     C*
029200010330     C                   if        §ctrok > 0
029300010601     C                             and invfil > *zeros
029400010330     C                   exsr      invio
029500010330     C                   endif
029600010601     C*
029700010618     C                   if        flgGiro = '1'
029800010601     C                   exsr      endela
029900010618     C                   endif
030000010330     C*
030100010330     C                   ENDSR
030200010330     C***
030300070601
030400070601     C*----------------------------------------------------*
030500070601     C*  SCARICAMENTO BUFFER RECORDS VAB
030600070601     C*----------------------------------------------------*
030700070601     C     WRIVAB        BEGSR
030800070601     C*
030900070601     C  N31              WRITE     FIVAB000
031000070601     C*
031100070601     C                   ENDSR
031200010601
031300010601
031400010601
031500010330     C*----------------------------------------------------*
031600020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
031700010330     C*----------------------------------------------------*
031800010330     C     INZVAR        BEGSR
031900010330     C*
032000020204     C                   Z-ADD     *zeros        Num5_0            5 0
032100020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
032200020322     C                   MOVEL     '0'           FlgCAS            1
032300010330     C*
032400010330     C                   ENDSR
032500010330     C*----------------------------------------------------*
032600020722     C*  IMPOSTAZIONE CAMPI COSTANTI
032700010330     C*----------------------------------------------------*
032800010330     C     DEFCAM        BEGSR
032900010330     C*
033000020204     C* Inizializzo il buffer del record da scrivere
033100050408     C                   CLEAR                   FIVAB000
033200020204     C* Imposto i valori di default...
033300070504     C                   EVAL      VABCCM = 0894106
033400070504     C                   EVAL      VABLNP = 089
033500060710     C                   EVAL      VABCTR = 000
033600070504     C                   EVAL      VABNAS = 'Hitech'
033700070504     C                   EVAL      VABTSP = 'C'
033800080826     C                   EVAL      VABCTM = '7T'
033900070208     C                   EVAL      VABCBO = '1'
034000020204     C* ... e poi verifico se sono stati passati come parametri
034100020204     C                   IF        vlrppt > *blanks
034200040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
034300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
034400020204     C                   EXSR      CHKNUM
034500020204     C                   IF        PiInt=*on
034600020204     C                   Z-ADD     PiVal         VABCCM
034700020204     C                   ENDIF
034800040301     C                   ENDIF
034900040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
035000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
035100020204     C                   EXSR      CHKNUM
035200020204     C                   IF        PiInt=*on
035300020204     C                   Z-ADD     PiVal         VABLNP
035400020204     C                   ENDIF
035500040301     C                   ENDIF
035600040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
035700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
035800020204     C                   EXSR      CHKNUM
035900020204     C                   IF        PiInt=*on
036000020204     C                   Z-ADD     PiVal         VABCTR
036100040301     C                   ENDIF
036200020204     C                   ENDIF
036300020204     C                   ENDIF
036400020204     C*
036500010330     C                   ENDSR
036600010607     C*----------------------------------------------------*
036700050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
036800010607     C*----------------------------------------------------*
036900010607     C     IMPVAB        BEGSR
037000010607     C*
037100010607     C                   SETOFF                                       3132
037200010607     C*
037300010607     C* Reperimento campi ALFA
037400010607     C*
037500010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
037600070504     C                   EVAL      VABRSD=%trim(%subst(vindta:164:27))
037700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
037800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
037900020117     C* ==
038000070504     C                   EVAL      VABRD2=%trim(%subst(vindta:191:35))
038100070504     C                   EVAL      VABIND=%trim(%subst(vindta:226:35))
038200070504     C                   EVAL      VABLOD=%trim(%subst(vindta:261:35))
038300070504     C                   EVAL      VABPRD=%trim(%subst(vindta:323:2))
038400070504     C                   EVAL      VABNOT=%trim(%subst(vindta:464:35))
038500070504     C                   EVAL      VABNT2=%trim(%subst(vindta:464+35:60-35))
038600070614     C                   EVAL      VABRMA=%trim(%subst(vindta:156:7))
038700070507     C                   IF        VABRMA = *blanks
038800070507     C                   EVAL      VABRMA=%trim(%subst(vindta:80:15))
038900070507     C                   ENDIF
039000070502     C*
039100070504     C* Considerazioni x determinazione tipo incasso contrassegno
039200070502     C                   SELECT
039300070504     C                   WHEN      %subst(vindta:524:1) = 'A'
039400070504     C                   EVAL      VABTIC = *blanks
039500070504     C                   WHEN      %subst(vindta:524:1) = 'B'
039600070504     C                   EVAL      VABTIC = 'BM'
039700070504     C                   WHEN      %subst(vindta:524:1) = 'C'
039800070504     C                   EVAL      VABTIC = 'CM'
039900070504     C                   WHEN      %subst(vindta:524:1) = 'D'
040000070504     C                   EVAL      VABTIC = *blanks
040100070504     C                   WHEN      %subst(vindta:524:1) = 'E'
040200070504     C                   EVAL      VABTIC = *blanks
040300070502     C                   ENDSL
040400060510     C*
040500060510     C* Reperimento campi NUMERICI
040600060510     C                   MOVEL     DATCOR        VABAAS
040700060510     C                   MOVE      DATCOR        VABMGS
040800060710     C* NSP/RMN
040900070504     C                   EVAL      PiStr=%trim(%subst(vindta:80:15))
041000060510     C                   EXSR      CHKNUM
041100060510     C                   IF        PiInt=*on
041200060510     C                   Z-ADD     PiVal         VABNSP
041300060510     C                   Z-ADD     PiVal         VABRMN
041400060404     C                   ELSE
041500070502     C                   SETON                                        31
041600060510     C                   Z-ADD     *zeros        VABNSP
041700060510     C                   Z-ADD     1             VABRMN
041800060404     C                   EVAL      vinmsg = %trimr(vinmsg)
041900060510     C                             + ' ' + 'VABNSP VABRMN'
042000060404     C                   ENDIF
042100040420     C* CAD
042200070504     C                   EVAL      PiStr=%trim(%subst(vindta:316:7))
042300010607     C                   EXSR      CHKNUM
042400010607     C                   IF        PiInt=*on
042500020204     C                   Z-ADD     PiVal         Num5_0
042600020204     C                   MOVEL(p)  Num5_0        VABCAD
042700010607     C                   ELSE
042800010607     C                   SETON                                        32
042900020204     C                   EVAL      VABCAD = *zeros
043000010607     C                   EVAL      vinmsg = %trimr(vinmsg)
043100020204     C                             + ' ' + 'VABCAD'
043200060316     C                   ENDIF
043300070502     C* Reperisco la provincia dal CAP e dalla località
043400070502     C                   IF        VABCAD <> *blanks AND
043500070502     C                             VABPRD  = *blanks
043600070502     C                   CLEAR                   TISI95DS
043700070502     C                   EVAL      I95TCN = '3'
043800070502     C                   Z-ADD     datcor        I95DAT
043900070502     C                   EVAL      I95CAP = VABCAD
044000070502     C                   EVAL      I95LOC = VABLOD
044100070502     C                   EVAL      I95NAR = VABNZD
044200070502     C                   CALL      'TISI95R'
044300070502     C                   PARM                    TISI95DS
044400070502     C                   EVAL      VABPRD = O95PRV
044500070502     C                   ENDIF
044600040420     C* NCL
044700070504     C                   EVAL      PiStr=%trim(%subst(vindta:355:5))
044800010607     C                   EXSR      CHKNUM
044900010607     C                   IF        PiInt=*on
045000070601     C                   ADD       PiVal         VABNCL
045100010607     C                   ELSE
045200010607     C                   SETON                                        32
045300010607     C                   Z-ADD     *zeros        VABNCL
045400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
045500010607     C                             + ' ' + 'VABNCL'
045600010607     C                   ENDIF
045700070504     C* DCR
045800070507     C                   IF        %subst(vindta:54:8) <> *blanks
045900070504     C                   EVAL      PiStr=%trim(%subst(vindta:54:8))
046000070504     C                   EXSR      CHKNUM
046100070504     C                   IF        PiInt=*on
046200070504     C                   Z-ADD     PiVal         VABDCR
046300070504     C                   ELSE
046400070504     C                   SETON                                        32
046500070504     C                   Z-ADD     *zeros        VABDCR
046600070504     C                   EVAL      vinmsg = %trimr(vinmsg)
046700070504     C                             + ' ' + 'VABDCR'
046800070504     C                   ENDIF
046900070507     C                   ENDIF
047000040420     C* PKB
047100070504     C                   EVAL      PiStr=%trim(%subst(vindta:360:7))
047200070208     C                   EVAL      PiDecChr = '.'
047300010607     C                   EXSR      CHKNUM
047400010607     C                   IF        PiNum=*on
047500070601     C                   EVAL(H)   PiVal = PiVal / 100                          * gest. 2 decimali
047600070601     C                   ADD       PiVal         VABPKB
047700010607     C                   ELSE
047800010607     C                   SETON                                        32
047900010607     C                   Z-ADD     *zeros        VABPKB
048000010607     C                   EVAL      vinmsg = %trimr(vinmsg)
048100010607     C                             + ' ' + 'VABPKB'
048200010607     C                   ENDIF
048300070504     C* VLB
048400070504     C                   EVAL      PiStr=%trim(%subst(vindta:385:7))
048500070504     C                   EVAL      PiDecChr = '.'
048600070504     C                   EXSR      CHKNUM
048700070504     C                   IF        PiNum=*on
048800070601     C                   EVAL(H)   PiVal = PiVal / 100                          * gest. 2 decimali
048900070601     C                   ADD       PiVal         VABVLB
049000070504     C                   ELSE
049100070504     C                   SETON                                        32
049200070504     C                   Z-ADD     *zeros        VABVLB
049300070504     C                   EVAL      vinmsg = %trimr(vinmsg)
049400070504     C                             + ' ' + 'VABVLB'
049500070504     C                   ENDIF
049600070504     C* CAS
049700070504     C                   IF        %trim(%subst(vindta:396:15)) <> *all'0'
049800070504     C                   EVAL      VABVCA = %trim(%subst(vindta:392:4))
049900070517     C                   EVAL      VABTIC = 'BM'
050000070504     C                   EVAL      FlgCAS = '1'
050100070504     C                   EVAL      PiStr=%trim(%subst(vindta:396:15))
050200070504     C                   EXSR      CHKNUM
050300070504     C                   IF        PiNum=*on
050400070601     C                   EVAL(H)   PiVal = PiVal / 1000                         * gest. 3 decimali
050500070601     C                   ADD       PiVal         VABCAS
050600070504     C                   ELSE
050700070504     C                   SETON                                        32
050800070504     C                   EVAL      vinmsg = %trimr(vinmsg)
050900070504     C                             + ' ' + 'VABCAS'
051000070504     C                   ENDIF
051100070504     C                   ENDIF
051200020322     C*
051300020322     C* Considerazioni finali su CBO/CAS
051400050908     C                   IF        FlgCAS = '1'
051500040713     C                   IF        VABCBO = '1'
051600020322     C                   EVAL      VABCBO = '4'
051700040713     C                   ENDIF
051800040713     C                   IF        VABCBO = '2'
051900040713     C                   EVAL      VABCBO = '6'
052000040713     C                   ENDIF
052100020322     C                   ENDIF
052200020204     C*
052300020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
052400020204     C                   EXSR      CHKIMPDIV
052500020204     C*
052600010607     C                   ENDSR
052700010607     C*----------------------------------------------------*
052800010601
052900020204
053000020204     C*----------------------------------------------------*
053100020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
053200020204     C*----------------------------------------------------*
053300020204     C     CHKIMPDIV     BEGSR
053400020204     C*
053500020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
053600020204     C                   Z-ADD     *zeros        wrkDec            9 9
053700020204     C*
053800020204     C* Come prima cosa effettuo considerazioni sulla divisa
053900020204     C                   IF        vabIAS > *zeros
054000020204     C                   IF        vabVAS <> 'EUR'
054100020204     C                   EVAL      vabVAS =  'ITL'
054200020204     C                   ENDIF
054300020204     C                   ENDIF
054400020204     C*
054500020204     C                   IF        vabCAS > *zeros
054600020204     C                   IF        vabVCA <> 'EUR'
054700020204     C                   EVAL      vabVCA =  'ITL'
054800020204     C                   ENDIF
054900020204     C                   ENDIF
055000020204     C*
055100020204     C                   IF        vabVMD > *zeros
055200020321     C                   IF        vabVAD <> 'EUR'
055300020204     C                   EVAL      vabVAD =  'ITL'
055400020204     C                   ENDIF
055500020204     C                   ENDIF
055600020204     C*
055700020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
055800020204     C                   Z-ADD     vabIAS        wrkDec
055900020204     C                   IF        wrkDec > *zeros
056000020204     C                   IF        vabVAS = 'ITL'
056100020204     C                   EVAL      vabIAS = *zeros
056200020204     C                   ENDIF
056300020204     C                   ENDIF
056400020204     C*
056500020204     C* Stabilisco se il contrasegno ha decimali valorizzati
056600020204     C                   Z-ADD     vabCAS        wrkDec
056700020204     C                   IF        wrkDec > *zeros
056800020204     C                   IF        vabVCA = 'ITL'
056900020204     C                   EVAL      vabCAS = *zeros
057000020204     C                   ENDIF
057100020204     C                   ENDIF
057200020204     C*
057300020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
057400020204     C                   Z-ADD     vabVMD        wrkDec
057500020204     C                   IF        wrkDec > *zeros
057600020204     C                   IF        vabVAD = 'ITL'
057700020204     C                   EVAL      vabVMD = *zeros
057800020204     C                   ENDIF
057900020204     C                   ENDIF
058000020204     C*
058100020204     C                   ENDSR
058200020204     C***
058300020204
058400010330
058500010330
058600010330     C*----------------------------------------------------*
058700010330     C*  CONTROLLO NUMERICITA' CAMPI
058800010330     C*----------------------------------------------------*
058900010330     C     CHKNUM        BEGSR
059000010330     C*
059100010606     C                   IF        PiDecChr = *blanks
059200070502     C                   EVAL      PiDecChr = '.'
059300010606     C                   ENDIF
059400010606     C*
059500010606     C                   CALL(e)   'ISNUMERIC'
059600010330     C                   PARM                    PiStr            30
059700010606     C                   PARM                    PiDecChr          1
059800010330     C                   PARM      *ZEROS        PiVal            30 9
059900010330     C                   PARM      '0'           PiInt             1
060000010330     C                   PARM      '0'           PiNum             1
060100010330     C                   IF        %error
060200010606     C                   EVAL      PiNum=*off
060300010330     C                   ENDIF
060400010330     C*
060500010330     C                   ENDSR
060600010330     C***
060700010330
060800010601
060900010601
061000010601
061100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
061200010601     C     repfil        BEGSR
061300010601     C*
061400010601     C                   if        invfil = *zeros and
061500010601     C                             depfil > *zeros and
061600010629     C                             (vinflg = *blanks or
061700010629     C                              vinflg = *zeros)
061800010601     C
061900010601     C                   eval      invfil = depfil
062000010601     C                   endif
062100010601     C*
062200010601     C                   if        depfil <> invfil and
062300010601     C                             invfil > *zeros
062400010601     C                   eval      flgMulti = '1'
062500010601     C                   if        vinflg = *blanks
062600010601     C                   add       1             cntNonEl
062700010601     C                   endif
062800010601     C                   endif
062900010601     C*
063000010601     C                   if        vinflg = '2'
063100010601     C                   eval      flgStato = '2'
063200010601     C                   endif
063300010601     C*
063400010601     C                   ENDSR
063500010601     C***
063600010601
063700010601
063800010601
063900010330
064000010330
064100010330
064200990920      /TITLE Invio dei dati al punto operativo.
064300000613     C     invio         BEGSR
064400990920     C*
064500990920     C                   reset                   dscmz
064600010601     C                   move      invfil        cmzdst
064700990920     C                   eval      cmzfld = vlrfou
064800990920     C                   eval      cmzmbd = vlrhdl
064900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
065000000710     C                   if        prmfir = *blanks
065100050408     C                   eval      cmzfla = 'FIVAB00F'
065200050408     C                   eval      cmzmba = 'FIVAB00F'
065300000710     C                   else
065400000710     C                   eval      cmzfla = prmfir
065500000710     C                   eval      cmzmba = prmfir
065600000710     C                   endif
065700990920     C                   eval      cmznrr = *zeros
065800990920     C                   move      §ctrok        cmznrr
065900021018     C                   eval      cmzlba = vlrfl1
066000990920     C                   call(e)   'TIS711C'
066100990920     C                   parm                    dscmz
066200990921     C                   parm      *blanks       esito
066300990920     C                   if        %error
066400990920     C                             or cmzerr = '1'
066500990921     C                             or esito  = '1'
066600000710     C                   eval      wrkesito = '3'
066700990920     C                   endif
066800990920     C*
066900000613     C                   ENDSR
067000990910
067100010601
067200010601
067300010601
067400010601
067500010601      /TITLE Invio dei dati al punto operativo.
067600010601     C     opeini        BEGSR
067700010601     C*
067800010601     C* Inizializzo flag e contatori operativi
067900010601     C                   movel     '0'           flgGiro           1
068000010601     C                   movel     '0'           flgMulti          1
068100010601     C                   movel     '1'           flgStato          1
068200010615     C                   movel     '0'           flgOk             1
068300010601     C                   z-add     *zeros        cntNonEl         10 0
068400010601     C                   z-add     *zeros        depfil            3 0
068500010601     C                   z-add     *zeros        invfil            3 0
068600010601     C*
068700010601     C                   ENDSR
068800010601     C***
068900010601
069000010601
069100010601
069200010330
069300010330
069400000613     C     *inzsr        BEGSR
069500990910     C*
069600990910     C     *entry        plist
069700990920     C                   parm                    tivlrds
069800990921     C                   parm      wrkesito      esito
069900000724     C                   parm                    prmlit
070000000710     C                   parm                    prmfir
070100010330     C*
070200010330     C* CALCOLA LA DATA CORRENTE
070300010330     C                   time                    wn14             14 0
070400010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
070500010330     C                   z-add     wn8           g08dat
070600010330     C                   z-add     *zeros        g08inv
070700010330     C                   movel     '0'           g08err
070800010330     C                   call      'XSRDA8'
070900010330     C                   parm                    wlbda8
071000010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
071100000613     C*
071200000613     C                   ENDSR
071300000613     C***
071400990908
071500070504     Otitvx2p   E            riepilogo         2
071600990915     O                                              'Upload via Internet'
071700990915     O                                           +1 'Traduzione TIVIN00R -'
071800050408     O                                           +1 'FIVABWWR'
071900990915     O                                           +1 'Testate bolle'
072000990915     O          E            riepilogo   2
072100990915     O                       wrkdata
072200990915     O                       wrkora              +1
072300990915     O                       procname            +1
072400990915     O          E            riepilogo   2
072500990915     O                                              'Cliente..................:'
072600010605     O                       VABCCM        z     +1
072700990915     O          E            riepilogo   2
072800990920     O                                              'Riferimento Strategi.....:'
072900990920     O                       vlrhdl              +1
073000990915     O          E            riepilogo   2
073100990915     O                                              'Giusti...................:'
073200971022     O                       §CTROK        2   +  1
073300990915     O          E            riepilogo   2
073400990915     O                                              'Sbagliati e corretti.....:'
073500971022     O                       §CTRMO        2   +  1
073600990915     O          E            riepilogo   2
073700990915     O                                              'Sbagliati e scartati.....:'
073800971022     O                       §CTRNO        2   +  1
073900000613
074000070504     Otitvx2ps  E            testdett          2
074100000613     O                                              'Upload via Internet'
074200000613     O                                           +1 'Traduzione TIVIN00R -'
074300050408     O                                           +1 'FIVABWWR'
074400000613     O                                           +1 'Testate bolle'
074500000616     O          E            testdett    3
074600000613     O                                           +2 'N° rec'
074700000613     O                                           +3 'Anteprima contenuto'
074800000616     O          E            rigadett    2
074900000613     O                       rrnum               +2
075000000621     O                       recko               +3
075100000616     O          E            findett     2
075200000613     O                       wrkdata
075300000613     O                       wrkora              +1
075400000613     O                       procname            +1
075500000616     O          E            findett     2
075600000613     O                                              'Cliente..................:'
075700010605     O                       VABCCM        z     +1
075800000616     O          E            findett     2
075900000613     O                                              'Riferimento Strategi.....:'
076000000613     O                       vlrhdl              +1
076100000616     O          E            findett     2
076200000613     O                                              'Giusti...................:'
076300000613     O                       §CTROK        2   +  1
076400000616     O          E            findett     2
076500000613     O                                              'Sbagliati e corretti.....:'
076600000613     O                       §CTRMO        2   +  1
076700000616     O          E            findett     2
076800000613     O                                              'Sbagliati e scartati.....:'
076900000613     O                       §CTRNO        2   +  1
077000000613** CMD - COMANDI CL
077100070504OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVX2P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
077200070504OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVX2PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
077300070504DLTOVR FILE(TITVX2P TITVX2PS) LVL(*)
077400010606
077500000613
