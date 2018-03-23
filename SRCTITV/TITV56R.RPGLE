000100030526      /TITLE Upload via Internet: traduzione in FIVABWWR. (x CARTORAMA - Cod. 2770533)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700030526     Ftitv56p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900030526     Ftitv56ps  O    f  198        PRINTER usropn
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
002300010330     D esito           s              1
002400010330     D prmlit          s             10
002500010330     D prmfir          s             10
002600010330     D wrkesito        s                   like(esito)
002700010330     D wrkdata         s               d
002800010330     D wrkora          s               t
002900010330     D rrnum           s              6  0 INZ(*zeros)
003000010330     D recko           s            150    INZ(*blanks)
003100011113     D depcmd          s            150    INZ(*blanks)
003200010330     D*------------------
003300010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003400010330     D*------------------
003500010330     D WLBDA8          DS                  INZ
003600010330     D  G08DAT                 1      8  0
003700010330     D  G08INV                 9     16  0
003800010330     D  G08ERR                17     17
003900010330     D  G08TGI                18     22  0
004000000313     D*
004100010330
004200010330
004300010330
004400990915     C                   time                    wrkdata
004500990915     C                   time                    wrkora
004600000913     C                   reset                   rrnum
004700990921     C                   reset                   esito
004800990921     C                   reset                   wrkesito
004900010601     C*
005000010601     C                   exsr      opeini
005100010605     C                   exsr      rwvab
005200010601     C*
005300010601     C                   seton                                        lr
005400010601
005500010601
005600010601
005700010601
005800010601     C*--------------------------------------------------------
005900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006000010601     C*--------------------------------------------------------
006100010601     C     PREELA        BEGSR
006200010601     C*
006300010601     C* SE OCCORRE SPEDIRE IN FILIALE
006400010601     C                   if        invfil <> *zeros and
006500010601     C                             flgGiro = '0'
006600010601     C*
006700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006800010601     C                   eval      flgGiro = '1'
006900000724     C*
007000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007200010601     C     invfil        chain     azorg01l
007300000724     C                   if        %found
007400000724     C                   movel(p)  CMD(1)        depcmd
007500020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007600000724     C*
007700000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007800011113     C                   Z-ADD     150           LENGH            15 5
007900000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008000000724     C                   PARM                    depcmd
008100000724     C                   PARM                    LENGH
008200000724     C*
008300000724     C                   endif
008400000616     C*
008500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008600000616     C                   movel(p)  CMD(2)        depcmd
008700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008800000616     C*
008900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009000011113     C                   Z-ADD     150           LENGH            15 5
009100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009200000616     C                   PARM                    depcmd
009300000616     C                   PARM                    LENGH
009400000613     C*
009500030526     C                   if        not %open(titv56ps)
009600030526     C                   open      titv56ps
009700000616     C                   except    testdett
009800000613     C                   endif
009900010601     C*
010000010601     C                   endif
010100010601     C*
010200010601     C                   ENDSR
010300010601     C***
010400010601
010500010601
010600010601
010700010601
010800010601
010900010601
011000010601
011100010601     C*--------------------------------------------------------
011200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011300010601     C*--------------------------------------------------------
011400010601     C     ENDELA        BEGSR
011500990915     C*
011600000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011700000613     C*
011800030526     C                   if        %open(titv56ps)
011900000616     C                   except    findett
012000030526     C                   close     titv56ps
012100000613     C                   endif
012200000616     C*
012300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012400011113     C                   Z-ADD     150           LENGH            15 5
012500010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012600000616     C                   PARM                    CMD(3)
012700000616     C                   PARM                    LENGH
012800000616     C*
012900010601     C                   ENDSR
013000010601     C***
013100010601
013200010601
013300010601
013400010601
013500000613
013600000613     C*--------------------------------------------------------
013700000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013800000613     C*--------------------------------------------------------
013900000621     C     STPR          BEGSR
014000000613     C*
014100030526     C                   if        not %open(titv56p)
014200030526     C                   open      titv56p
014300990915     C                   endif
014400990915     C*
014500990915     C                   except    riepilogo
014600990915     C*
014700030526     C                   if        %open(titv56p)
014800030526     C                   close     titv56p
014900990914     C                   endif
015000000613     C*
015100000613     C                   ENDSR
015200000613     C***
015300000613
015400010330
015500010601
015600010601
015700010601
015800010330     C*--------------------------------------------------------
015900021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
016000010330     C*--------------------------------------------------------
016100010605     C     RWVAB         BEGSR
016200010330     C*
016300010330     C                   if        not %open(tivin00r)
016400010330     C                   open      tivin00r
016500010330     C                   endif
016600021025     C                   if        not %open(fivabwwr)
016700021025     C                   open      fivabwwr
016800010330     C                   endif
016900010330     C*
017000010604     C                   clear                   §CTROK
017100010604     C                   clear                   §CTRMO
017200010604     C                   clear                   §CTRNO
017300010330     C*
017400010330     C                   DO        *HIVAL
017500010330     C*
017600010330     C                   READ      tivin00r                               70
017700010618     C*
017800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017900010618     C                   if        vinflg = '1'
018000010618     C                   eval      flgOk = '1'
018100010618     C                   endif
018200010618     C*
018300010330     C                   if        vindta > *blanks
018400010330     C                   add       1             rrnum
018500010330     C*
018600010601     C                   if        *in70 = *off and
018700010330     C                             (vinflg = *blanks
018800010330     C                              or vinflg = '0'
018900010330     C                              or vinflg = '2')
019000010330     C*
019100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019200010711     C                   if        vinflg = *blanks or vinflg = '0'
019300010711     C                   clear                   vinmsg
019400010711     C                   endif
019500010601     C*
019600010330     C                   exsr      inzvar
019700010330     C                   exsr      defcam
019800010605     C                   exsr      impvab
019900010601     C*
020000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020100010605     C                   eval      depfil = VABLNP
020200010601     C                   exsr      repfil
020300010601     C                   if        depfil = invfil
020400021025     C                   if        vlrpoi = 999
020500021025     C                   MOVE(P)   invfil        VABFGS
020600021025     C                   else
020700021025     C                   MOVE(P)   vlrpoi        VABFGS
020800021025     C                   endif
020900010601     C*
021000010601     C                   exsr      PREELA
021100010601     C*
021200010604     C* Ebbene...
021300010604     C*
021400010604     C  N31              ADD       1             §CTROK            7 0
021500010604     C   32              ADD       1             §CTRMO            7 0
021600010604     C   31              ADD       1             §CTRNO            7 0
021700021025     C  N31              WRITE     FIVAB000
021800010604     C*
021900010604     C                   if        *in31 = *off and
022000010604     C                             *in32 = *off
022100010604     C                   eval      vinflg = '1'
022200010604     C                   else
022300010604     C                   eval      recko = vindta
022400010604     C                   except    rigadett
022500010604     C                   eval      vinflg = '2'
022600010604     C                   endif
022700010604     C                   endif
022800010601     C*
022900010604     C                   endif
023000010604     C*
023100010330     C                   else
023200010330     C                   eval      vinflg = '1'
023300010330     C                   endif
023400010601     C*
023500010601     C  N70              update    tivin000
023600010330     C*
023700010330     C  N70              ENDdo
023800010601     C
023900010601     C*
024000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024100010601     C                   if        cntNonEl = *zeros or
024200010601     C                             flgMulti = '0'
024300010330     C* Se non ci sono record con errori ...
024400010601     C                   if        §ctrno = 0 and
024500010604     C                             §ctrmo = 0 and
024600010601     C                             flgStato <> '2'
024700010330     C* ... restituisco esito OK.
024800010330     C                   eval      wrkesito = '0'
024900010330     C                   else
025000010330     C                   if        §ctrok > 0
025100010330     C                   eval      wrkesito = '1'
025200010330     C                   else
025300010615     C                   if        flgOk = '0'
025400010615     C                   eval      wrkesito = '2'
025500010615     C                   else
025600010615     C                   eval      wrkesito = '6'
025700010615     C                   endif
025800010330     C                   endif
025900010330     C                   endif
026000010601     C                   else
026100010601     C                   eval      wrkesito = '9'
026200010601     C                   endif
026300010330     C*
026400010330     C                   if        %open(tivin00r)
026500010330     C                   close     tivin00r
026600010330     C                   endif
026700021025     C                   if        %open(fivabwwr)
026800021025     C                   close     fivabwwr
026900010330     C                   endif
027000010601     C*
027100010601     C                   if        vlrpoi <> 999
027200010601     C                   eval      invfil = vlrpoi
027300010601     C                   endif
027400010330     C*
027500010330     C                   if        §ctrok > 0
027600010601     C                             and invfil > *zeros
027700010330     C                   exsr      invio
027800010330     C                   endif
027900010601     C*
028000010618     C                   if        flgGiro = '1'
028100010601     C                   exsr      endela
028200010618     C                   endif
028300010330     C*
028400010330     C                   ENDSR
028500010330     C***
028600010601
028700010601
028800010601
028900010330     C*----------------------------------------------------*
029000010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029100010330     C*----------------------------------------------------*
029200010330     C     INZVAR        BEGSR
029300010330     C*
029400020204     C                   Z-ADD     *zeros        Num5_0            5 0
029500010330     C*
029600010330     C                   ENDSR
029700010330     C*----------------------------------------------------*
029800010330     C*  IMPOSTAZIONE CAMPI COSTANTI
029900010330     C*----------------------------------------------------*
030000010330     C     DEFCAM        BEGSR
030100010330     C*
030200020204     C* Inizializzo il buffer del record da scrivere
030300021025     C                   CLEAR                   FIVAB000
030400020204     C* Imposto i valori di default...
030500030526     C                   EVAL      VABCCM = 2770533
030600030526     C                   EVAL      VABLNP = 277
030700030526     C                   EVAL      VABCTR = 000
030800020204     C* ... e poi verifico se sono stati passati come parametri
030900020204     C                   IF        vlrppt > *blanks
031000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031100020204     C                   EXSR      CHKNUM
031200020204     C                   IF        PiInt=*on
031300020204     C                   Z-ADD     PiVal         VABCCM
031400020204     C                   ENDIF
031500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031600020204     C                   EXSR      CHKNUM
031700020204     C                   IF        PiInt=*on
031800020204     C                   Z-ADD     PiVal         VABLNP
031900020204     C                   ENDIF
032000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032100020204     C                   EXSR      CHKNUM
032200020204     C                   IF        PiInt=*on
032300020204     C                   Z-ADD     PiVal         VABCTR
032400020204     C                   ENDIF
032500020204     C                   ENDIF
032600020204     C*
032700010330     C                   ENDSR
032800010607     C*----------------------------------------------------*
032900021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033000010607     C*----------------------------------------------------*
033100010607     C     IMPVAB        BEGSR
033200010607     C*
033300010607     C                   SETOFF                                       3132
033400030515     C*
033500010607     C                   MOVEL     datcor        VABAAS
033600010607     C                   MOVE      datcor        VABMGS
033700010607     C*
033800010607     C* Reperimento campi ALFA
033900010607     C*
034000010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
034100030515     C                   EVAL      VABRSD=%trim(%subst(vindta:18:30))
034200020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034300020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034400020117     C* ==
034500030515     C                   EVAL      VABIND=%trim(%subst(vindta:48:30))
034600030515     C                   EVAL      VABLOD=%trim(%subst(vindta:78:30))
034700030515     C                   EVAL      VABPRD=%trim(%subst(vindta:108:30))
034800030526     C                   EVAL      VABRMA=%trim(%subst(vindta:2:10))
034900030526     C                   EVAL      VABCBO=%trim(%subst(vindta:1:1))
035000010607     C*
035100010607     C* Reperimento campi NUMERICI
035200020604     C*
035300030515     C* CAD
035400030515     C                   EVAL      PiStr=%trim(%subst(vindta:110:5))
035500010607     C                   EXSR      CHKNUM
035600010607     C                   IF        PiInt=*on
035700020204     C                   Z-ADD     PiVal         Num5_0
035800020204     C                   MOVEL(p)  Num5_0        VABCAD
035900010607     C                   ELSE
036000010607     C                   SETON                                        32
036100020204     C                   EVAL      VABCAD = *zeros
036200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
036300020204     C                             + ' ' + 'VABCAD'
036400010607     C                   ENDIF
036500030515     C* RMN/NSP
036600030526     C                   EVAL      PiStr=%trim(%subst(vindta:2:10))
036700011023     C                   EXSR      CHKNUM
036800011023     C                   IF        PiInt=*on
036900030225     C                   Z-ADD     PiVal         VABRMN
037000030225     C                   Z-ADD     PiVal         VABNSP
037100011023     C                   ELSE
037200020205     C                   EVAL      VABRMN = 1
037300011023     C                   ENDIF
037400030515     C* NCL
037500030515     C                   EVAL      PiStr=%trim(%subst(vindta:115:5))
037600010607     C                   EXSR      CHKNUM
037700010607     C                   IF        PiInt=*on
037800010607     C                   Z-ADD     PiVal         VABNCL
037900010607     C                   ELSE
038000010607     C                   SETON                                        32
038100010607     C                   Z-ADD     *zeros        VABNCL
038200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
038300010607     C                             + ' ' + 'VABNCL'
038400010607     C                   ENDIF
038500030515     C* PKB
038600030515     C                   EVAL      PiStr=%trim(%subst(vindta:120:7))
038700010607     C                   EXSR      CHKNUM
038800010607     C                   IF        PiNum=*on
038900030515     C                   EVAL      PiVal = PiVal / 100                          xchè c sono 2 decim.
039000010607     C                   Z-ADD(H)  PiVal         VABPKB
039100010607     C                   ELSE
039200010607     C                   SETON                                        32
039300010607     C                   Z-ADD     *zeros        VABPKB
039400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039500010607     C                             + ' ' + 'VABPKB'
039600010607     C                   ENDIF
039700020204     C*
039800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
039900020204     C                   EXSR      CHKIMPDIV
040000020204     C*
040100010607     C                   ENDSR
040200010607     C*----------------------------------------------------*
040300010601
040400020204
040500020204     C*----------------------------------------------------*
040600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
040700020204     C*----------------------------------------------------*
040800020204     C     CHKIMPDIV     BEGSR
040900020204     C*
041000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
041100020204     C                   Z-ADD     *zeros        wrkDec            9 9
041200020204     C*
041300020204     C* Come prima cosa effettuo considerazioni sulla divisa
041400020204     C                   IF        vabIAS > *zeros
041500020204     C                   IF        vabVAS <> 'EUR'
041600020204     C                   EVAL      vabVAS =  'ITL'
041700020204     C                   ENDIF
041800020204     C                   ENDIF
041900020204     C*
042000020204     C                   IF        vabCAS > *zeros
042100020204     C                   IF        vabVCA <> 'EUR'
042200020204     C                   EVAL      vabVCA =  'ITL'
042300020204     C                   ENDIF
042400020204     C                   ENDIF
042500020204     C*
042600020204     C                   IF        vabVMD > *zeros
042700020204     C                   IF        vabVAD <> 'EUR'
042800020204     C                   EVAL      vabVAD =  'ITL'
042900020204     C                   ENDIF
043000020204     C                   ENDIF
043100020204     C*
043200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
043300020204     C                   Z-ADD     vabIAS        wrkDec
043400020204     C                   IF        wrkDec > *zeros
043500020204     C                   IF        vabVAS = 'ITL'
043600020204     C                   EVAL      vabIAS = *zeros
043700020204     C                   ENDIF
043800020204     C                   ENDIF
043900020204     C*
044000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
044100020204     C                   Z-ADD     vabCAS        wrkDec
044200020204     C                   IF        wrkDec > *zeros
044300020204     C                   IF        vabVCA = 'ITL'
044400020204     C                   EVAL      vabCAS = *zeros
044500020204     C                   ENDIF
044600020204     C                   ENDIF
044700020204     C*
044800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
044900020204     C                   Z-ADD     vabVMD        wrkDec
045000020204     C                   IF        wrkDec > *zeros
045100020204     C                   IF        vabVAD = 'ITL'
045200020204     C                   EVAL      vabVMD = *zeros
045300020204     C                   ENDIF
045400020204     C                   ENDIF
045500020204     C*
045600020204     C                   ENDSR
045700020204     C***
045800020204
045900010330
046000010330
046100010330     C*----------------------------------------------------*
046200010330     C*  CONTROLLO NUMERICITA' CAMPI
046300010330     C*----------------------------------------------------*
046400010330     C     CHKNUM        BEGSR
046500010330     C*
046600010606     C                   IF        PiDecChr = *blanks
046700020624     C                   EVAL      PiDecChr = '.'
046800010606     C                   ENDIF
046900010606     C*
047000010606     C                   CALL(e)   'ISNUMERIC'
047100010330     C                   PARM                    PiStr            30
047200010606     C                   PARM                    PiDecChr          1
047300010330     C                   PARM      *ZEROS        PiVal            30 9
047400010330     C                   PARM      '0'           PiInt             1
047500010330     C                   PARM      '0'           PiNum             1
047600010330     C                   IF        %error
047700010606     C                   EVAL      PiNum=*off
047800010330     C                   ENDIF
047900010330     C*
048000010330     C                   ENDSR
048100010330     C***
048200010330
048300010601
048400010601
048500010601
048600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
048700010601     C     repfil        BEGSR
048800010601     C*
048900010601     C                   if        invfil = *zeros and
049000010601     C                             depfil > *zeros and
049100010629     C                             (vinflg = *blanks or
049200010629     C                              vinflg = *zeros)
049300010601     C
049400010601     C                   eval      invfil = depfil
049500010601     C                   endif
049600010601     C*
049700010601     C                   if        depfil <> invfil and
049800010601     C                             invfil > *zeros
049900010601     C                   eval      flgMulti = '1'
050000010601     C                   if        vinflg = *blanks
050100010601     C                   add       1             cntNonEl
050200010601     C                   endif
050300010601     C                   endif
050400010601     C*
050500010601     C                   if        vinflg = '2'
050600010601     C                   eval      flgStato = '2'
050700010601     C                   endif
050800010601     C*
050900010601     C                   ENDSR
051000010601     C***
051100010601
051200010601
051300010601
051400010330
051500010330
051600010330
051700990920      /TITLE Invio dei dati al punto operativo.
051800000613     C     invio         BEGSR
051900990920     C*
052000990920     C                   reset                   dscmz
052100010601     C                   move      invfil        cmzdst
052200990920     C                   eval      cmzfld = vlrfou
052300990920     C                   eval      cmzmbd = vlrhdl
052400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
052500000710     C                   if        prmfir = *blanks
052600021025     C                   eval      cmzfla = 'FIVAB00F'
052700021025     C                   eval      cmzmba = 'FIVAB00F'
052800000710     C                   else
052900000710     C                   eval      cmzfla = prmfir
053000000710     C                   eval      cmzmba = prmfir
053100000710     C                   endif
053200990920     C                   eval      cmznrr = *zeros
053300990920     C                   move      §ctrok        cmznrr
053400021018     C                   eval      cmzlba = vlrfl1
053500990920     C                   call(e)   'TIS711C'
053600990920     C                   parm                    dscmz
053700990921     C                   parm      *blanks       esito
053800990920     C                   if        %error
053900990920     C                             or cmzerr = '1'
054000990921     C                             or esito  = '1'
054100000710     C                   eval      wrkesito = '3'
054200990920     C                   endif
054300990920     C*
054400000613     C                   ENDSR
054500990910
054600010601
054700010601
054800010601
054900010601
055000010601      /TITLE Invio dei dati al punto operativo.
055100010601     C     opeini        BEGSR
055200010601     C*
055300010601     C* Inizializzo flag e contatori operativi
055400010601     C                   movel     '0'           flgGiro           1
055500010601     C                   movel     '0'           flgMulti          1
055600010601     C                   movel     '1'           flgStato          1
055700010615     C                   movel     '0'           flgOk             1
055800010601     C                   z-add     *zeros        cntNonEl         10 0
055900010601     C                   z-add     *zeros        depfil            3 0
056000010601     C                   z-add     *zeros        invfil            3 0
056100010601     C*
056200010601     C                   ENDSR
056300010601     C***
056400010601
056500010601
056600010601
056700010330
056800010330
056900000613     C     *inzsr        BEGSR
057000990910     C*
057100990910     C     *entry        plist
057200990920     C                   parm                    tivlrds
057300990921     C                   parm      wrkesito      esito
057400000724     C                   parm                    prmlit
057500000710     C                   parm                    prmfir
057600010330     C*
057700010330     C* CALCOLA LA DATA CORRENTE
057800010330     C                   time                    wn14             14 0
057900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
058000010330     C                   z-add     wn8           g08dat
058100010330     C                   z-add     *zeros        g08inv
058200010330     C                   movel     '0'           g08err
058300010330     C                   call      'XSRDA8'
058400010330     C                   parm                    wlbda8
058500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
058600000613     C*
058700000613     C                   ENDSR
058800000613     C***
058900990908
059000030526     Otitv56p   E            riepilogo         2
059100990915     O                                              'Upload via Internet'
059200990915     O                                           +1 'Traduzione TIVIN00R -'
059300021025     O                                           +1 'FIVABWWR'
059400990915     O                                           +1 'Testate bolle'
059500990915     O          E            riepilogo   2
059600990915     O                       wrkdata
059700990915     O                       wrkora              +1
059800990915     O                       procname            +1
059900990915     O          E            riepilogo   2
060000990915     O                                              'Cliente..................:'
060100010605     O                       VABCCM        z     +1
060200990915     O          E            riepilogo   2
060300990920     O                                              'Riferimento Strategi.....:'
060400990920     O                       vlrhdl              +1
060500990915     O          E            riepilogo   2
060600990915     O                                              'Giusti...................:'
060700971022     O                       §CTROK        2   +  1
060800990915     O          E            riepilogo   2
060900990915     O                                              'Sbagliati e corretti.....:'
061000971022     O                       §CTRMO        2   +  1
061100990915     O          E            riepilogo   2
061200990915     O                                              'Sbagliati e scartati.....:'
061300971022     O                       §CTRNO        2   +  1
061400000613
061500030526     Otitv56ps  E            testdett          2
061600000613     O                                              'Upload via Internet'
061700000613     O                                           +1 'Traduzione TIVIN00R -'
061800021025     O                                           +1 'FIVABWWR'
061900000613     O                                           +1 'Testate bolle'
062000000616     O          E            testdett    3
062100000613     O                                           +2 'N° rec'
062200000613     O                                           +3 'Anteprima contenuto'
062300000616     O          E            rigadett    2
062400000613     O                       rrnum               +2
062500000621     O                       recko               +3
062600000616     O          E            findett     2
062700000613     O                       wrkdata
062800000613     O                       wrkora              +1
062900000613     O                       procname            +1
063000000616     O          E            findett     2
063100000613     O                                              'Cliente..................:'
063200010605     O                       VABCCM        z     +1
063300000616     O          E            findett     2
063400000613     O                                              'Riferimento Strategi.....:'
063500000613     O                       vlrhdl              +1
063600000616     O          E            findett     2
063700000613     O                                              'Giusti...................:'
063800000613     O                       §CTROK        2   +  1
063900000616     O          E            findett     2
064000000613     O                                              'Sbagliati e corretti.....:'
064100000613     O                       §CTRMO        2   +  1
064200000616     O          E            findett     2
064300000613     O                                              'Sbagliati e scartati.....:'
064400000613     O                       §CTRNO        2   +  1
064500000613** CMD - COMANDI CL
064600030526OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV56P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
064700030526OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV56PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
064800030526DLTOVR FILE(TITV56P TITV56PS) LVL(*)
064900010606
065000000613
