000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050408     FFIVABWWR  O    E             DISK    usropn
000700071003     Ftitv1b2p  O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900071003     Ftitv1b2ps O    f  198        PRINTER usropn
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
010000071003     C                   if        not %open(titv1b2ps)
010100071003     C                   open      titv1b2ps
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
011900071003     C                   if        %open(titv1b2ps)
012000000616     C                   except    findett
012100071003     C                   close     titv1b2ps
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
014000071003     C                   if        not %open(titv1b2p)
014100071003     C                   open      titv1b2p
014200990915     C                   endif
014300990915     C*
014400990915     C                   except    riepilogo
014500990915     C*
014600071003     C                   if        %open(titv1b2p)
014700071003     C                   close     titv1b2p
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
028700070601     C*
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
031200071003     C                   EVAL      VABCCM = 0961067
031300071003     C                   EVAL      VABLNP = 096
031400060710     C                   EVAL      VABCTR = 000
031500071003     C                   EVAL      VABTSP = 'C'
031600070208     C                   EVAL      VABCBO = '1'
031700020204     C* ... e poi verifico se sono stati passati come parametri
031800020204     C                   IF        vlrppt > *blanks
031900040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
032000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
032100020204     C                   EXSR      CHKNUM
032200020204     C                   IF        PiInt=*on
032300020204     C                   Z-ADD     PiVal         VABCCM
032400020204     C                   ENDIF
032500040301     C                   ENDIF
032600040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
032700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032800020204     C                   EXSR      CHKNUM
032900020204     C                   IF        PiInt=*on
033000020204     C                   Z-ADD     PiVal         VABLNP
033100020204     C                   ENDIF
033200040301     C                   ENDIF
033300040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
033400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
033500020204     C                   EXSR      CHKNUM
033600020204     C                   IF        PiInt=*on
033700020204     C                   Z-ADD     PiVal         VABCTR
033800040301     C                   ENDIF
033900020204     C                   ENDIF
034000020204     C                   ENDIF
034100020204     C*
034200010330     C                   ENDSR
034300010607     C*----------------------------------------------------*
034400050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
034500010607     C*----------------------------------------------------*
034600010607     C     IMPVAB        BEGSR
034700010607     C*
034800010607     C                   SETOFF                                       3132
034900010607     C*
035000010607     C* Reperimento campi ALFA
035100010607     C*
035200010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
035300071003     C                   EVAL      VABRSD=%trim(%subst(vindta:155:35))
035400020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035500020117     C     '@':'A'       XLATE     VABRSD        VABRSD
035600020117     C* ==
035700071003     C                   EVAL      VABRD2=%trim(%subst(vindta:155+35:70-35))
035800071003     C                   EVAL      VABIND=%trim(%subst(vindta:225:35))
035900071003     C                   EVAL      VABLOD=%trim(%subst(vindta:260:55))
036000071003     C                   EVAL      VABPRD=%trim(%subst(vindta:322:2))
036100071003     C                   EVAL      VABRMA=%trim(%subst(vindta:83:6))
036200060510     C*
036300060510     C* Reperimento campi NUMERICI
036400060510     C                   MOVEL     DATCOR        VABAAS
036500060510     C                   MOVE      DATCOR        VABMGS
036600060710     C* NSP/RMN
036700071003     C                   EVAL      PiStr=%trim(%subst(vindta:83:6))
036800060510     C                   EXSR      CHKNUM
036900060510     C                   IF        PiInt=*on
037000060510     C                   Z-ADD     PiVal         VABNSP
037100060510     C                   Z-ADD     PiVal         VABRMN
037200060404     C                   ELSE
037300070502     C                   SETON                                        31
037400060510     C                   Z-ADD     *zeros        VABNSP
037500060510     C                   Z-ADD     1             VABRMN
037600060404     C                   EVAL      vinmsg = %trimr(vinmsg)
037700060510     C                             + ' ' + 'VABNSP VABRMN'
037800060404     C                   ENDIF
037900040420     C* CAD
038000071003     C                   EVAL      PiStr=%trim(%subst(vindta:315:5))
038100010607     C                   EXSR      CHKNUM
038200010607     C                   IF        PiInt=*on
038300020204     C                   Z-ADD     PiVal         Num5_0
038400020204     C                   MOVEL(p)  Num5_0        VABCAD
038500010607     C                   ELSE
038600010607     C                   SETON                                        32
038700020204     C                   EVAL      VABCAD = *zeros
038800010607     C                   EVAL      vinmsg = %trimr(vinmsg)
038900020204     C                             + ' ' + 'VABCAD'
039000060316     C                   ENDIF
039100070502     C* Reperisco la provincia dal CAP e dalla località
039200070502     C                   IF        VABCAD <> *blanks AND
039300070502     C                             VABPRD  = *blanks
039400070502     C                   CLEAR                   TISI95DS
039500070502     C                   EVAL      I95TCN = '3'
039600070502     C                   Z-ADD     datcor        I95DAT
039700070502     C                   EVAL      I95CAP = VABCAD
039800070502     C                   EVAL      I95LOC = VABLOD
039900070502     C                   EVAL      I95NAR = VABNZD
040000070502     C                   CALL      'TISI95R'
040100070502     C                   PARM                    TISI95DS
040200070502     C                   EVAL      VABPRD = O95PRV
040300070502     C                   ENDIF
040400040420     C* NCL
040500071003     C                   EVAL      PiStr=%trim(%subst(vindta:351:7))
040600010607     C                   EXSR      CHKNUM
040700010607     C                   IF        PiInt=*on
040800070606     C                   Z-ADD     PiVal         VABNCL
040900010607     C                   ELSE
041000010607     C                   SETON                                        32
041100010607     C                   Z-ADD     *zeros        VABNCL
041200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
041300010607     C                             + ' ' + 'VABNCL'
041400010607     C                   ENDIF
041500040420     C* PKB
041600071003     C                   EVAL      PiStr=%trim(%subst(vindta:358:6))
041700010607     C                   EXSR      CHKNUM
041800010607     C                   IF        PiNum=*on
041900071003     C                   EVAL      PiVal = PiVal / 10                           * gestisco 1 dec.
042000071003     C                   Z-ADD(H)  PiVal         VABPKB
042100010607     C                   ELSE
042200010607     C                   SETON                                        32
042300010607     C                   Z-ADD     *zeros        VABPKB
042400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
042500010607     C                             + ' ' + 'VABPKB'
042600010607     C                   ENDIF
042700020322     C*
042800020322     C* Considerazioni finali su CBO/CAS
042900050908     C                   IF        FlgCAS = '1'
043000040713     C                   IF        VABCBO = '1'
043100020322     C                   EVAL      VABCBO = '4'
043200040713     C                   ENDIF
043300040713     C                   IF        VABCBO = '2'
043400040713     C                   EVAL      VABCBO = '6'
043500040713     C                   ENDIF
043600020322     C                   ENDIF
043700020204     C*
043800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
043900020204     C                   EXSR      CHKIMPDIV
044000020204     C*
044100010607     C                   ENDSR
044200010607     C*----------------------------------------------------*
044300010601
044400020204
044500020204     C*----------------------------------------------------*
044600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
044700020204     C*----------------------------------------------------*
044800020204     C     CHKIMPDIV     BEGSR
044900020204     C*
045000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
045100020204     C                   Z-ADD     *zeros        wrkDec            9 9
045200020204     C*
045300020204     C* Come prima cosa effettuo considerazioni sulla divisa
045400020204     C                   IF        vabIAS > *zeros
045500020204     C                   IF        vabVAS <> 'EUR'
045600020204     C                   EVAL      vabVAS =  'ITL'
045700020204     C                   ENDIF
045800020204     C                   ENDIF
045900020204     C*
046000020204     C                   IF        vabCAS > *zeros
046100020204     C                   IF        vabVCA <> 'EUR'
046200020204     C                   EVAL      vabVCA =  'ITL'
046300020204     C                   ENDIF
046400020204     C                   ENDIF
046500020204     C*
046600020204     C                   IF        vabVMD > *zeros
046700020321     C                   IF        vabVAD <> 'EUR'
046800020204     C                   EVAL      vabVAD =  'ITL'
046900020204     C                   ENDIF
047000020204     C                   ENDIF
047100020204     C*
047200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
047300020204     C                   Z-ADD     vabIAS        wrkDec
047400020204     C                   IF        wrkDec > *zeros
047500020204     C                   IF        vabVAS = 'ITL'
047600020204     C                   EVAL      vabIAS = *zeros
047700020204     C                   ENDIF
047800020204     C                   ENDIF
047900020204     C*
048000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
048100020204     C                   Z-ADD     vabCAS        wrkDec
048200020204     C                   IF        wrkDec > *zeros
048300020204     C                   IF        vabVCA = 'ITL'
048400020204     C                   EVAL      vabCAS = *zeros
048500020204     C                   ENDIF
048600020204     C                   ENDIF
048700020204     C*
048800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
048900020204     C                   Z-ADD     vabVMD        wrkDec
049000020204     C                   IF        wrkDec > *zeros
049100020204     C                   IF        vabVAD = 'ITL'
049200020204     C                   EVAL      vabVMD = *zeros
049300020204     C                   ENDIF
049400020204     C                   ENDIF
049500020204     C*
049600020204     C                   ENDSR
049700020204     C***
049800020204
049900010330
050000010330
050100010330     C*----------------------------------------------------*
050200010330     C*  CONTROLLO NUMERICITA' CAMPI
050300010330     C*----------------------------------------------------*
050400010330     C     CHKNUM        BEGSR
050500010330     C*
050600010606     C                   IF        PiDecChr = *blanks
050700070502     C                   EVAL      PiDecChr = '.'
050800010606     C                   ENDIF
050900010606     C*
051000010606     C                   CALL(e)   'ISNUMERIC'
051100010330     C                   PARM                    PiStr            30
051200010606     C                   PARM                    PiDecChr          1
051300010330     C                   PARM      *ZEROS        PiVal            30 9
051400010330     C                   PARM      '0'           PiInt             1
051500010330     C                   PARM      '0'           PiNum             1
051600010330     C                   IF        %error
051700010606     C                   EVAL      PiNum=*off
051800010330     C                   ENDIF
051900010330     C*
052000010330     C                   ENDSR
052100010330     C***
052200010330
052300010601
052400010601
052500010601
052600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
052700010601     C     repfil        BEGSR
052800010601     C*
052900010601     C                   if        invfil = *zeros and
053000010601     C                             depfil > *zeros and
053100010629     C                             (vinflg = *blanks or
053200010629     C                              vinflg = *zeros)
053300010601     C
053400010601     C                   eval      invfil = depfil
053500010601     C                   endif
053600010601     C*
053700010601     C                   if        depfil <> invfil and
053800010601     C                             invfil > *zeros
053900010601     C                   eval      flgMulti = '1'
054000010601     C                   if        vinflg = *blanks
054100010601     C                   add       1             cntNonEl
054200010601     C                   endif
054300010601     C                   endif
054400010601     C*
054500010601     C                   if        vinflg = '2'
054600010601     C                   eval      flgStato = '2'
054700010601     C                   endif
054800010601     C*
054900010601     C                   ENDSR
055000010601     C***
055100010601
055200010601
055300010601
055400010330
055500010330
055600010330
055700990920      /TITLE Invio dei dati al punto operativo.
055800000613     C     invio         BEGSR
055900990920     C*
056000990920     C                   reset                   dscmz
056100010601     C                   move      invfil        cmzdst
056200990920     C                   eval      cmzfld = vlrfou
056300990920     C                   eval      cmzmbd = vlrhdl
056400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
056500000710     C                   if        prmfir = *blanks
056600050408     C                   eval      cmzfla = 'FIVAB00F'
056700050408     C                   eval      cmzmba = 'FIVAB00F'
056800000710     C                   else
056900000710     C                   eval      cmzfla = prmfir
057000000710     C                   eval      cmzmba = prmfir
057100000710     C                   endif
057200990920     C                   eval      cmznrr = *zeros
057300990920     C                   move      §ctrok        cmznrr
057400021018     C                   eval      cmzlba = vlrfl1
057500990920     C                   call(e)   'TIS711C'
057600990920     C                   parm                    dscmz
057700990921     C                   parm      *blanks       esito
057800990920     C                   if        %error
057900990920     C                             or cmzerr = '1'
058000990921     C                             or esito  = '1'
058100000710     C                   eval      wrkesito = '3'
058200990920     C                   endif
058300990920     C*
058400000613     C                   ENDSR
058500990910
058600010601
058700010601
058800010601
058900010601
059000010601      /TITLE Invio dei dati al punto operativo.
059100010601     C     opeini        BEGSR
059200010601     C*
059300010601     C* Inizializzo flag e contatori operativi
059400010601     C                   movel     '0'           flgGiro           1
059500010601     C                   movel     '0'           flgMulti          1
059600010601     C                   movel     '1'           flgStato          1
059700010615     C                   movel     '0'           flgOk             1
059800010601     C                   z-add     *zeros        cntNonEl         10 0
059900010601     C                   z-add     *zeros        depfil            3 0
060000010601     C                   z-add     *zeros        invfil            3 0
060100010601     C*
060200010601     C                   ENDSR
060300010601     C***
060400010601
060500010601
060600010601
060700010330
060800010330
060900000613     C     *inzsr        BEGSR
061000990910     C*
061100990910     C     *entry        plist
061200990920     C                   parm                    tivlrds
061300990921     C                   parm      wrkesito      esito
061400000724     C                   parm                    prmlit
061500000710     C                   parm                    prmfir
061600010330     C*
061700010330     C* CALCOLA LA DATA CORRENTE
061800010330     C                   time                    wn14             14 0
061900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
062000010330     C                   z-add     wn8           g08dat
062100010330     C                   z-add     *zeros        g08inv
062200010330     C                   movel     '0'           g08err
062300010330     C                   call      'XSRDA8'
062400010330     C                   parm                    wlbda8
062500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
062600000613     C*
062700000613     C                   ENDSR
062800000613     C***
062900990908
063000071003     Otitv1b2p  E            riepilogo         2
063100990915     O                                              'Upload via Internet'
063200990915     O                                           +1 'Traduzione TIVIN00R -'
063300050408     O                                           +1 'FIVABWWR'
063400990915     O                                           +1 'Testate bolle'
063500990915     O          E            riepilogo   2
063600990915     O                       wrkdata
063700990915     O                       wrkora              +1
063800990915     O                       procname            +1
063900990915     O          E            riepilogo   2
064000990915     O                                              'Cliente..................:'
064100010605     O                       VABCCM        z     +1
064200990915     O          E            riepilogo   2
064300990920     O                                              'Riferimento Strategi.....:'
064400990920     O                       vlrhdl              +1
064500990915     O          E            riepilogo   2
064600990915     O                                              'Giusti...................:'
064700971022     O                       §CTROK        2   +  1
064800990915     O          E            riepilogo   2
064900990915     O                                              'Sbagliati e corretti.....:'
065000971022     O                       §CTRMO        2   +  1
065100990915     O          E            riepilogo   2
065200990915     O                                              'Sbagliati e scartati.....:'
065300971022     O                       §CTRNO        2   +  1
065400000613
065500071003     Otitv1b2ps E            testdett          2
065600000613     O                                              'Upload via Internet'
065700000613     O                                           +1 'Traduzione TIVIN00R -'
065800050408     O                                           +1 'FIVABWWR'
065900000613     O                                           +1 'Testate bolle'
066000000616     O          E            testdett    3
066100000613     O                                           +2 'N° rec'
066200000613     O                                           +3 'Anteprima contenuto'
066300000616     O          E            rigadett    2
066400000613     O                       rrnum               +2
066500000621     O                       recko               +3
066600000616     O          E            findett     2
066700000613     O                       wrkdata
066800000613     O                       wrkora              +1
066900000613     O                       procname            +1
067000000616     O          E            findett     2
067100000613     O                                              'Cliente..................:'
067200010605     O                       VABCCM        z     +1
067300000616     O          E            findett     2
067400000613     O                                              'Riferimento Strategi.....:'
067500000613     O                       vlrhdl              +1
067600000616     O          E            findett     2
067700000613     O                                              'Giusti...................:'
067800000613     O                       §CTROK        2   +  1
067900000616     O          E            findett     2
068000000613     O                                              'Sbagliati e corretti.....:'
068100000613     O                       §CTRMO        2   +  1
068200000616     O          E            findett     2
068300000613     O                                              'Sbagliati e scartati.....:'
068400000613     O                       §CTRNO        2   +  1
068500000613** CMD - COMANDI CL
068600071003OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1B2P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
068700071003OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1B2PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
068800071003DLTOVR FILE(TITV1B2P TITV1B2PS) LVL(*)
068900010606
069000000613
