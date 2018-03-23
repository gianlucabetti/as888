000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050408     FFIVABWWR  O    E             DISK    usropn
000700080408     Ftitv1f1p  O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900080408     Ftitv1f1ps O    f  198        PRINTER usropn
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
003300010330     D*------------------
003400010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003500010330     D*------------------
003600010330     D WLBDA8          DS                  INZ
003700010330     D  G08DAT                 1      8  0
003800010330     D  G08INV                 9     16  0
003900010330     D  G08ERR                17     17
004000010330     D  G08TGI                18     22  0
004100010330
004200010330
004300990915     C                   time                    wrkdata
004400990915     C                   time                    wrkora
004500000913     C                   reset                   rrnum
004600990921     C                   reset                   esito
004700990921     C                   reset                   wrkesito
004800010601     C*
004900010601     C                   exsr      opeini
005000010605     C                   exsr      rwvab
005100070502     C*
005200070502     C* Effettuo lancio TISI95 solo x chiusura
005300070502     C                   CLEAR                   TISI95DS
005400070502     C                   EVAL      I95TLA = 'C'
005500070502     C                   CALL      'TISI95R'
005600070502     C                   PARM                    TISI95DS
005700010601     C*
005800010601     C                   seton                                        lr
005900010601
006000010601
006100010601
006200010601
006300010601     C*--------------------------------------------------------
006400010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006500010601     C*--------------------------------------------------------
006600010601     C     PREELA        BEGSR
006700010601     C*
006800010601     C* SE OCCORRE SPEDIRE IN FILIALE
006900010601     C                   if        invfil <> *zeros and
007000010601     C                             flgGiro = '0'
007100010601     C*
007200010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007300010601     C                   eval      flgGiro = '1'
007400000724     C*
007500000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007600000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007700010601     C     invfil        chain     azorg01l
007800000724     C                   if        %found
007900000724     C                   movel(p)  CMD(1)        depcmd
008000020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008100000724     C*
008200000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008300011113     C                   Z-ADD     150           LENGH            15 5
008400000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008500000724     C                   PARM                    depcmd
008600000724     C                   PARM                    LENGH
008700000724     C*
008800000724     C                   endif
008900000616     C*
009000000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009100000616     C                   movel(p)  CMD(2)        depcmd
009200000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009300000616     C*
009400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009500011113     C                   Z-ADD     150           LENGH            15 5
009600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009700000616     C                   PARM                    depcmd
009800000616     C                   PARM                    LENGH
009900000613     C*
010000080408     C                   if        not %open(titv1f1ps)
010100080408     C                   open      titv1f1ps
010200000616     C                   except    testdett
010300000613     C                   endif
010400010601     C*
010500010601     C                   endif
010600010601     C*
010700010601     C                   ENDSR
010800010601     C***
010900010601
011000010601
011100010601
011200010601     C*--------------------------------------------------------
011300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011400010601     C*--------------------------------------------------------
011500010601     C     ENDELA        BEGSR
011600990915     C*
011700000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011800000613     C*
011900080408     C                   if        %open(titv1f1ps)
012000000616     C                   except    findett
012100080408     C                   close     titv1f1ps
012200000613     C                   endif
012300000616     C*
012400000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012500011113     C                   Z-ADD     150           LENGH            15 5
012600010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012700000616     C                   PARM                    CMD(3)
012800000616     C                   PARM                    LENGH
012900000616     C*
013000010601     C                   ENDSR
013100010601     C***
013200010601
013300010601
013400000613
013500000613     C*--------------------------------------------------------
013600000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013700000613     C*--------------------------------------------------------
013800000621     C     STPR          BEGSR
013900000613     C*
014000080408     C                   if        not %open(titv1f1p)
014100080408     C                   open      titv1f1p
014200990915     C                   endif
014300990915     C*
014400990915     C                   except    riepilogo
014500990915     C*
014600080408     C                   if        %open(titv1f1p)
014700080408     C                   close     titv1f1p
014800990914     C                   endif
014900000613     C*
015000000613     C                   ENDSR
015100000613     C***
015200000613
015300010601
015400010601
015500010330     C*--------------------------------------------------------
015600050408     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWR              *
015700010330     C*--------------------------------------------------------
015800010605     C     RWVAB         BEGSR
015900050408     C*
016000010330     C                   if        not %open(tivin00r)
016100010330     C                   open      tivin00r
016200010330     C                   endif
016300050408     C                   if        not %open(fivabwwr)
016400050408     C                   open      fivabwwr
016500010330     C                   endif
016600010330     C*
016700010604     C                   clear                   §CTROK
016800010604     C                   clear                   §CTRMO
016900010604     C                   clear                   §CTRNO
017000010330     C*
017100010330     C                   DO        *HIVAL
017200010330     C*
017300010330     C                   READ      tivin00r                               70
017400010618     C*
017500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017600010618     C                   if        vinflg = '1'
017700010618     C                   eval      flgOk = '1'
017800010618     C                   endif
017900010618     C*
018000010330     C                   if        vindta > *blanks
018100010330     C                   add       1             rrnum
018200010330     C*
018300010601     C                   if        *in70 = *off and
018400010330     C                             (vinflg = *blanks
018500010330     C                              or vinflg = '0'
018600010330     C                              or vinflg = '2')
018700010330     C*
018800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018900010711     C                   if        vinflg = *blanks or vinflg = '0'
019000010711     C                   clear                   vinmsg
019100010711     C                   endif
019200070601     C*
019300070601     C                   exsr      inzvar
019400070601     C                   exsr      defcam
019500070601     C                   exsr      impvab
019600010601     C*
019700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019800010605     C                   eval      depfil = VABLNP
019900010601     C                   exsr      repfil
020000010601     C                   if        depfil = invfil
020100021025     C                   if        vlrpoi = 999
020200021025     C                   MOVE(P)   invfil        VABFGS
020300021025     C                   else
020400021025     C                   MOVE(P)   vlrpoi        VABFGS
020500021025     C                   endif
020600010601     C*
020700010601     C                   exsr      PREELA
020800010601     C*
020900010604     C* Ebbene...
021000010604     C*
021100010604     C  N31              ADD       1             §CTROK            7 0
021200010604     C   32              ADD       1             §CTRMO            7 0
021300010604     C   31              ADD       1             §CTRNO            7 0
021400071003     C                   exsr      wrivab                                       => scrivo VAB
021500010604     C*
021600020722     C                   endif
021700020722     C*
021800010604     C                   if        *in31 = *off and
021900010604     C                             *in32 = *off
022000010604     C                   eval      vinflg = '1'
022100010604     C                   else
022200010604     C                   eval      recko = vindta
022300010604     C                   except    rigadett
022400010604     C                   eval      vinflg = '2'
022500010604     C                   endif
022600010604     C                   endif
022700010604     C*
022800010330     C                   else
022900010330     C                   eval      vinflg = '1'
023000010330     C                   endif
023100010601     C*
023200010601     C  N70              update    tivin000
023300010330     C*
023400010330     C  N70              ENDdo
023500010601     C*
023600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023700010601     C                   if        cntNonEl = *zeros or
023800010601     C                             flgMulti = '0'
023900010330     C* Se non ci sono record con errori ...
024000010601     C                   if        §ctrno = 0 and
024100010604     C                             §ctrmo = 0 and
024200010601     C                             flgStato <> '2'
024300010330     C* ... restituisco esito OK.
024400010330     C                   eval      wrkesito = '0'
024500010330     C                   else
024600010330     C                   if        §ctrok > 0
024700010330     C                   eval      wrkesito = '1'
024800010330     C                   else
024900010615     C                   if        flgOk = '0'
025000010615     C                   eval      wrkesito = '2'
025100010615     C                   else
025200010615     C                   eval      wrkesito = '6'
025300010615     C                   endif
025400010330     C                   endif
025500010330     C                   endif
025600010601     C                   else
025700010601     C                   eval      wrkesito = '9'
025800010601     C                   endif
025900010330     C*
026000010330     C                   if        %open(tivin00r)
026100010330     C                   close     tivin00r
026200010330     C                   endif
026300050408     C                   if        %open(fivabwwr)
026400050408     C                   close     fivabwwr
026500010330     C                   endif
026600010601     C*
026700010601     C                   if        vlrpoi <> 999
026800010601     C                   eval      invfil = vlrpoi
026900010601     C                   endif
027000010330     C*
027100010330     C                   if        §ctrok > 0
027200010601     C                             and invfil > *zeros
027300010330     C                   exsr      invio
027400010330     C                   endif
027500010601     C*
027600010618     C                   if        flgGiro = '1'
027700010601     C                   exsr      endela
027800010618     C                   endif
027900010330     C*
028000010330     C                   ENDSR
028100010330     C***
028200070601
028300070601     C*----------------------------------------------------*
028400070601     C*  SCARICAMENTO BUFFER RECORDS VAB
028500070601     C*----------------------------------------------------*
028600070601     C     WRIVAB        BEGSR
028700080321     C*
028800070601     C  N31              WRITE     FIVAB000
028900070601     C*
029000070601     C                   ENDSR
029100010601
029200010601
029300010601
029400010330     C*----------------------------------------------------*
029500020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029600010330     C*----------------------------------------------------*
029700010330     C     INZVAR        BEGSR
029800010330     C*
029900020204     C                   Z-ADD     *zeros        Num5_0            5 0
030000020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
030100020322     C                   MOVEL     '0'           FlgCAS            1
030200010330     C*
030300010330     C                   ENDSR
030400010330     C*----------------------------------------------------*
030500020722     C*  IMPOSTAZIONE CAMPI COSTANTI
030600010330     C*----------------------------------------------------*
030700010330     C     DEFCAM        BEGSR
030800010330     C*
030900020204     C* Inizializzo il buffer del record da scrivere
031000050408     C                   CLEAR                   FIVAB000
031100020204     C* Imposto i valori di default...
031200080408     C                   EVAL      VABCCM = 0054081
031300080408     C                   EVAL      VABLNP = 005
031400080307     C                   EVAL      VABCTR = 000
031500070208     C                   EVAL      VABCBO = '1'
031600020204     C* ... e poi verifico se sono stati passati come parametri
031700020204     C                   IF        vlrppt > *blanks
031800040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
031900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
032000020204     C                   EXSR      CHKNUM
032100020204     C                   IF        PiInt=*on
032200020204     C                   Z-ADD     PiVal         VABCCM
032300020204     C                   ENDIF
032400040301     C                   ENDIF
032500040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
032600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032700020204     C                   EXSR      CHKNUM
032800020204     C                   IF        PiInt=*on
032900020204     C                   Z-ADD     PiVal         VABLNP
033000020204     C                   ENDIF
033100040301     C                   ENDIF
033200040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
033300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
033400020204     C                   EXSR      CHKNUM
033500020204     C                   IF        PiInt=*on
033600020204     C                   Z-ADD     PiVal         VABCTR
033700040301     C                   ENDIF
033800020204     C                   ENDIF
033900020204     C                   ENDIF
034000020204     C*
034100010330     C                   ENDSR
034200010607     C*----------------------------------------------------*
034300050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
034400010607     C*----------------------------------------------------*
034500010607     C     IMPVAB        BEGSR
034600010607     C*
034700010607     C                   SETOFF                                       3132
034800010607     C*
034900010607     C* Reperimento campi ALFA
035000080408     C                   EVAL      VABRMO=%trim(%subst(vindta:107:30))
035100080408     C                   EVAL      VABRSD=%trim(%subst(vindta:236:30))
035200080408     C                   EVAL      VABIND=%trim(%subst(vindta:266:30))
035300080408     C                   EVAL      VABLOD=%trim(%subst(vindta:301:25))
035400080408     C                   EVAL      VABPRD=%trim(%subst(vindta:326:2))
035500080409     C                   EVAL      VABRMA=%trim(%subst(vindta:44:12))
035600080408     C                   EVAL      VABNOT=%trim(%subst(vindta:328:30))
035700080408     C                   EVAL      VABNT2=%trim(%subst(vindta:358:30))
035800080408     C*
035900080408     C* Considerazioni sulla ragione sociale del destinatario da indicare
036000080408     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
036100080408     C     '@':'A'       XLATE     VABRSD        VABRSD
036200080408     C* ==
036300080321     C*
036400080410     C* TIC
036500080408     C                   IF        %subst(vindta:623:1) = 'C'
036600080408     C                   EVAL      VABTIC = 'OC'
036700080321     C                   ENDIF
036800080408     C                   IF        %subst(vindta:623:1) = 'I'
036900080408     C                   EVAL      VABTIC = 'OM'
037000080408     C                   ENDIF
037100080408     C* CCM/CTR
037200080408     C                   IF        %subst(vindta:44:2) = 'RF'
037300080408     C                   EVAL      VABCCM = 0054082
037400080408     C                   EVAL      VABCTR = 100
037500080408     C                   ENDIF
037600060510     C*
037700060510     C* Reperimento campi NUMERICI
037800060510     C                   MOVEL     DATCOR        VABAAS
037900060510     C                   MOVE      DATCOR        VABMGS
038000060710     C* NSP/RMN
038100080409     C                   EVAL      PiStr=%trim(%subst(vindta:48:8))
038200060510     C                   EXSR      CHKNUM
038300060510     C                   IF        PiInt=*on
038400060510     C                   Z-ADD     PiVal         VABNSP
038500060510     C                   Z-ADD     PiVal         VABRMN
038600060404     C                   ELSE
038700070502     C                   SETON                                        31
038800060510     C                   Z-ADD     *zeros        VABNSP
038900060510     C                   Z-ADD     1             VABRMN
039000060404     C                   EVAL      vinmsg = %trimr(vinmsg)
039100060510     C                             + ' ' + 'VABNSP VABRMN'
039200060404     C                   ENDIF
039300080408     C* CAD
039400080408     C                   EVAL      PiStr=%trim(%subst(vindta:296:5))
039500080408     C                   EXSR      CHKNUM
039600080408     C                   IF        PiInt=*on
039700080408     C                   Z-ADD     PiVal         Num5_0
039800080408     C                   MOVEL(p)  Num5_0        VABCAD
039900080408     C                   ELSE
040000080408     C                   SETON                                        32
040100080408     C                   EVAL      VABCAD = *zeros
040200080408     C                   EVAL      vinmsg = %trimr(vinmsg)
040300080408     C                             + ' ' + 'VABCAD'
040400080408     C                   ENDIF
040500080408     C* Reperisco la provincia dal CAP e dalla località
040600080408     C                   IF        VABCAD <> *blanks AND
040700080408     C                             VABPRD  = *blanks
040800080408     C                   CLEAR                   TISI95DS
040900080408     C                   EVAL      I95TCN = '3'
041000080408     C                   Z-ADD     datcor        I95DAT
041100080408     C                   EVAL      I95CAP = VABCAD
041200080408     C                   EVAL      I95LOC = VABLOD
041300080408     C                   EVAL      I95NAR = VABNZD
041400080408     C                   CALL      'TISI95R'
041500080408     C                   PARM                    TISI95DS
041600080408     C                   EVAL      VABPRD = O95PRV
041700080408     C                   ENDIF
041800080320     C* DCR
041900080409     C                   IF        %subst(vindta:638:6) <> *blanks AND
042000080409     C                             %subst(vindta:638:6) <> *zeros
042100080408     C                   EVAL      PiStr='20'+%trim(%subst(vindta:638:6))
042200080320     C                   EXSR      CHKNUM
042300080320     C                   IF        PiInt=*on
042400080320     C                   Z-ADD     PiVal         VABDCR
042500080320     C                   ELSE
042600080320     C                   SETON                                        32
042700080320     C                   Z-ADD     *zeros        VABDCR
042800080320     C                   EVAL      vinmsg = %trimr(vinmsg)
042900080320     C                             + ' ' + 'VABDCR'
043000080320     C                   ENDIF
043100080409     C                   ENDIF
043200040420     C* NCL
043300080408     C                   EVAL      PiStr=%trim(%subst(vindta:400:6))
043400010607     C                   EXSR      CHKNUM
043500010607     C                   IF        PiInt=*on
043600070606     C                   Z-ADD     PiVal         VABNCL
043700010607     C                   ELSE
043800010607     C                   SETON                                        32
043900010607     C                   Z-ADD     *zeros        VABNCL
044000010607     C                   EVAL      vinmsg = %trimr(vinmsg)
044100010607     C                             + ' ' + 'VABNCL'
044200010607     C                   ENDIF
044300071029     C* PKB
044400080408     C                   EVAL      PiStr=%trim(%subst(vindta:406:7))
044500010607     C                   EXSR      CHKNUM
044600010607     C                   IF        PiNum=*on
044700080408     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
044800071003     C                   Z-ADD(H)  PiVal         VABPKB
044900010607     C                   ELSE
045000010607     C                   SETON                                        32
045100010607     C                   Z-ADD     *zeros        VABPKB
045200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
045300010607     C                             + ' ' + 'VABPKB'
045400010607     C                   ENDIF
045500080320     C* CAS
045600080408     C                   IF        %subst(vindta:63:10)<>'0000000.00'
045700080408     C                   EVAL      FlgCAS = '1'
045800080408     C                   EVAL      VABVCA = 'EUR'
045900080408     C                   EVAL      PiStr=%trim(%subst(vindta:63:10))
046000080320     C                   EXSR      CHKNUM
046100080320     C                   IF        PiNum=*on
046200080320     C                   Z-ADD(H)  PiVal         VABCAS
046300080320     C                   ELSE
046400080320     C                   SETON                                        32
046500080320     C                   Z-ADD     *zeros        VABCAS
046600080320     C                   EVAL      vinmsg = %trimr(vinmsg)
046700080320     C                             + ' ' + 'VABCAS'
046800080408     C                   ENDIF
046900080320     C                   ENDIF
047000020322     C*
047100020322     C* Considerazioni finali su CBO/CAS
047200050908     C                   IF        FlgCAS = '1'
047300040713     C                   IF        VABCBO = '1'
047400020322     C                   EVAL      VABCBO = '4'
047500040713     C                   ENDIF
047600040713     C                   IF        VABCBO = '2'
047700040713     C                   EVAL      VABCBO = '6'
047800040713     C                   ENDIF
047900020322     C                   ENDIF
048000020204     C*
048100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
048200020204     C                   EXSR      CHKIMPDIV
048300020204     C*
048400010607     C                   ENDSR
048500010607     C*----------------------------------------------------*
048600010601
048700020204
048800020204     C*----------------------------------------------------*
048900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
049000020204     C*----------------------------------------------------*
049100020204     C     CHKIMPDIV     BEGSR
049200020204     C*
049300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
049400020204     C                   Z-ADD     *zeros        wrkDec            9 9
049500020204     C*
049600020204     C* Come prima cosa effettuo considerazioni sulla divisa
049700020204     C                   IF        vabIAS > *zeros
049800020204     C                   IF        vabVAS <> 'EUR'
049900020204     C                   EVAL      vabVAS =  'ITL'
050000020204     C                   ENDIF
050100020204     C                   ENDIF
050200020204     C*
050300020204     C                   IF        vabCAS > *zeros
050400020204     C                   IF        vabVCA <> 'EUR'
050500020204     C                   EVAL      vabVCA =  'ITL'
050600020204     C                   ENDIF
050700020204     C                   ENDIF
050800020204     C*
050900020204     C                   IF        vabVMD > *zeros
051000020321     C                   IF        vabVAD <> 'EUR'
051100020204     C                   EVAL      vabVAD =  'ITL'
051200020204     C                   ENDIF
051300020204     C                   ENDIF
051400020204     C*
051500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
051600020204     C                   Z-ADD     vabIAS        wrkDec
051700020204     C                   IF        wrkDec > *zeros
051800020204     C                   IF        vabVAS = 'ITL'
051900020204     C                   EVAL      vabIAS = *zeros
052000020204     C                   ENDIF
052100020204     C                   ENDIF
052200020204     C*
052300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
052400020204     C                   Z-ADD     vabCAS        wrkDec
052500020204     C                   IF        wrkDec > *zeros
052600020204     C                   IF        vabVCA = 'ITL'
052700020204     C                   EVAL      vabCAS = *zeros
052800020204     C                   ENDIF
052900020204     C                   ENDIF
053000020204     C*
053100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
053200020204     C                   Z-ADD     vabVMD        wrkDec
053300020204     C                   IF        wrkDec > *zeros
053400020204     C                   IF        vabVAD = 'ITL'
053500020204     C                   EVAL      vabVMD = *zeros
053600020204     C                   ENDIF
053700020204     C                   ENDIF
053800020204     C*
053900020204     C                   ENDSR
054000020204     C***
054100020204
054200010330
054300010330
054400010330     C*----------------------------------------------------*
054500010330     C*  CONTROLLO NUMERICITA' CAMPI
054600010330     C*----------------------------------------------------*
054700010330     C     CHKNUM        BEGSR
054800010330     C*
054900010606     C                   IF        PiDecChr = *blanks
055000080409     C                   EVAL      PiDecChr = '.'
055100010606     C                   ENDIF
055200010606     C*
055300010606     C                   CALL(e)   'ISNUMERIC'
055400010330     C                   PARM                    PiStr            30
055500010606     C                   PARM                    PiDecChr          1
055600010330     C                   PARM      *ZEROS        PiVal            30 9
055700010330     C                   PARM      '0'           PiInt             1
055800010330     C                   PARM      '0'           PiNum             1
055900010330     C                   IF        %error
056000010606     C                   EVAL      PiNum=*off
056100010330     C                   ENDIF
056200010330     C*
056300010330     C                   ENDSR
056400010330     C***
056500010330
056600010601
056700010601
056800010601
056900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
057000010601     C     repfil        BEGSR
057100010601     C*
057200010601     C                   if        invfil = *zeros and
057300010601     C                             depfil > *zeros and
057400010629     C                             (vinflg = *blanks or
057500010629     C                              vinflg = *zeros)
057600010601     C
057700010601     C                   eval      invfil = depfil
057800010601     C                   endif
057900010601     C*
058000010601     C                   if        depfil <> invfil and
058100010601     C                             invfil > *zeros
058200010601     C                   eval      flgMulti = '1'
058300010601     C                   if        vinflg = *blanks
058400010601     C                   add       1             cntNonEl
058500010601     C                   endif
058600010601     C                   endif
058700010601     C*
058800010601     C                   if        vinflg = '2'
058900010601     C                   eval      flgStato = '2'
059000010601     C                   endif
059100010601     C*
059200010601     C                   ENDSR
059300010601     C***
059400010601
059500010601
059600010601
059700010330
059800010330
059900010330
060000990920      /TITLE Invio dei dati al punto operativo.
060100000613     C     invio         BEGSR
060200990920     C*
060300990920     C                   reset                   dscmz
060400010601     C                   move      invfil        cmzdst
060500990920     C                   eval      cmzfld = vlrfou
060600990920     C                   eval      cmzmbd = vlrhdl
060700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
060800000710     C                   if        prmfir = *blanks
060900050408     C                   eval      cmzfla = 'FIVAB00F'
061000050408     C                   eval      cmzmba = 'FIVAB00F'
061100000710     C                   else
061200000710     C                   eval      cmzfla = prmfir
061300000710     C                   eval      cmzmba = prmfir
061400000710     C                   endif
061500990920     C                   eval      cmznrr = *zeros
061600990920     C                   move      §ctrok        cmznrr
061700021018     C                   eval      cmzlba = vlrfl1
061800990920     C                   call(e)   'TIS711C'
061900990920     C                   parm                    dscmz
062000990921     C                   parm      *blanks       esito
062100990920     C                   if        %error
062200990920     C                             or cmzerr = '1'
062300990921     C                             or esito  = '1'
062400000710     C                   eval      wrkesito = '3'
062500990920     C                   endif
062600990920     C*
062700000613     C                   ENDSR
062800990910
062900010601
063000010601
063100010601
063200010601
063300010601      /TITLE Invio dei dati al punto operativo.
063400010601     C     opeini        BEGSR
063500010601     C*
063600010601     C* Inizializzo flag e contatori operativi
063700010601     C                   movel     '0'           flgGiro           1
063800010601     C                   movel     '0'           flgMulti          1
063900010601     C                   movel     '1'           flgStato          1
064000010615     C                   movel     '0'           flgOk             1
064100010601     C                   z-add     *zeros        cntNonEl         10 0
064200010601     C                   z-add     *zeros        depfil            3 0
064300010601     C                   z-add     *zeros        invfil            3 0
064400010601     C*
064500010601     C                   ENDSR
064600010601     C***
064700010601
064800010601
064900010601
065000010330
065100010330
065200000613     C     *inzsr        BEGSR
065300990910     C*
065400990910     C     *entry        plist
065500990920     C                   parm                    tivlrds
065600990921     C                   parm      wrkesito      esito
065700000724     C                   parm                    prmlit
065800000710     C                   parm                    prmfir
065900010330     C*
066000010330     C* CALCOLA LA DATA CORRENTE
066100010330     C                   time                    wn14             14 0
066200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
066300010330     C                   z-add     wn8           g08dat
066400010330     C                   z-add     *zeros        g08inv
066500010330     C                   movel     '0'           g08err
066600010330     C                   call      'XSRDA8'
066700010330     C                   parm                    wlbda8
066800010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
066900000613     C*
067000000613     C                   ENDSR
067100000613     C***
067200990908
067300080408     Otitv1f1p  E            riepilogo         2
067400990915     O                                              'Upload via Internet'
067500990915     O                                           +1 'Traduzione TIVIN00R -'
067600050408     O                                           +1 'FIVABWWR'
067700990915     O                                           +1 'Testate bolle'
067800990915     O          E            riepilogo   2
067900990915     O                       wrkdata
068000990915     O                       wrkora              +1
068100990915     O                       procname            +1
068200990915     O          E            riepilogo   2
068300990915     O                                              'Cliente..................:'
068400010605     O                       VABCCM        z     +1
068500990915     O          E            riepilogo   2
068600990920     O                                              'Riferimento Strategi.....:'
068700990920     O                       vlrhdl              +1
068800990915     O          E            riepilogo   2
068900990915     O                                              'Giusti...................:'
069000971022     O                       §CTROK        2   +  1
069100990915     O          E            riepilogo   2
069200990915     O                                              'Sbagliati e corretti.....:'
069300971022     O                       §CTRMO        2   +  1
069400990915     O          E            riepilogo   2
069500990915     O                                              'Sbagliati e scartati.....:'
069600971022     O                       §CTRNO        2   +  1
069700000613
069800080408     Otitv1f1ps E            testdett          2
069900000613     O                                              'Upload via Internet'
070000000613     O                                           +1 'Traduzione TIVIN00R -'
070100050408     O                                           +1 'FIVABWWR'
070200000613     O                                           +1 'Testate bolle'
070300000616     O          E            testdett    3
070400000613     O                                           +2 'N° rec'
070500000613     O                                           +3 'Anteprima contenuto'
070600000616     O          E            rigadett    2
070700000613     O                       rrnum               +2
070800000621     O                       recko               +3
070900000616     O          E            findett     2
071000000613     O                       wrkdata
071100000613     O                       wrkora              +1
071200000613     O                       procname            +1
071300000616     O          E            findett     2
071400000613     O                                              'Cliente..................:'
071500010605     O                       VABCCM        z     +1
071600000616     O          E            findett     2
071700000613     O                                              'Riferimento Strategi.....:'
071800000613     O                       vlrhdl              +1
071900000616     O          E            findett     2
072000000613     O                                              'Giusti...................:'
072100000613     O                       §CTROK        2   +  1
072200000616     O          E            findett     2
072300000613     O                                              'Sbagliati e corretti.....:'
072400000613     O                       §CTRMO        2   +  1
072500000616     O          E            findett     2
072600000613     O                                              'Sbagliati e scartati.....:'
072700000613     O                       §CTRNO        2   +  1
072800000613** CMD - COMANDI CL
072900080408OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1F1P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
073000080408OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1F1PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
073100080408DLTOVR FILE(TITV1F1P TITV1F1PS) LVL(*)
073200010606
073300000613
